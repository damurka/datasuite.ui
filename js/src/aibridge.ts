// The AI bridge's page side (docs/AI-BRIDGE.md, protocol 2): window.datasuite, through which DataSuite's chat reads
// where the user is (getState) and asks the app for things (request). The app's R side (R/kit-ai-bridge.R) publishes
// what R knows -- the page, its components (charts and tables) with the output they draw into, what the app adds --
// with "datasuite_ai_state", and answers requests with "datasuite_ai_reply". What only the browser knows -- the
// cards, their tabs and which one is showing, what is in view, what has been drawn, titles -- is measured here each
// time getState() runs, so it is right even after the user scrolled or switched a tab without R hearing of it.
//
// getState, listComponents, focusComponent and selectTab are answered here (they are about the page); every other
// action goes to R. request() never rejects: every failure is a reply { ok: false, error } a person can read.
//
// The "Ask AI" buttons go the other way: askAi() tells R what the user asked about ("datasuite_ask_ai"); R writes
// the prompt and asks DataSuite to open its chat with it (R/kit-ai-bridge.R). What was asked about also goes in the
// state (askedAbout), so the chat knows which chart "this" is.

type Reply = { ok: true; result: unknown } | { ok: false; error: string };

interface Pending {
  resolve: (reply: Reply) => void;
  timer: ReturnType<typeof setTimeout>;
}

interface RComponent {
  id: string;
  type: "chart" | "table";
  outputId: string;
  about?: unknown;
}

interface RState {
  protocol: number;
  components?: RComponent[];
  [key: string]: unknown;
}

type InView = "full" | "partial" | "none";

interface Tab {
  key: string;
  label: string;
  componentId?: string;
}

interface Card {
  id: string;
  title: string;
  inView: InView;
  visibleFraction: number;
  activeTab?: string;
  tabs: Tab[];
}

interface Component {
  id: string;
  cardId: string;
  tabKey?: string;
  type: "chart" | "table";
  title: string;
  drawn: boolean;
  about?: unknown;
}

interface DatasuiteBridge {
  protocol: 2;
  getState(): unknown;
  request(action: string, args?: Record<string, unknown>, options?: { timeoutMs?: number }): Promise<Reply>;
}

declare global {
  interface Window {
    datasuite?: DatasuiteBridge;
  }
}

let rState: RState | null = null;

// What the user last pressed an Ask AI button for.
interface AskedAbout {
  scope: "page" | "card";
  cardId?: string;
  componentId?: string;
  title?: string;
  at: string;
}
let askedAbout: AskedAbout | null = null;
const pending = new Map<string, Pending>();
let counter = 0;

function shinyConnected(): boolean {
  const shiny = window.Shiny as { setInputValue?: unknown; shinyapp?: { isConnected?: () => boolean } } | undefined;
  if (!shiny || typeof shiny.setInputValue !== "function") return false;
  const isConnected = shiny.shinyapp?.isConnected;
  return typeof isConnected === "function" ? isConnected.call(shiny.shinyapp) : true;
}

function settle(id: string, reply: Reply): void {
  const entry = pending.get(id);
  if (!entry) return;
  pending.delete(id);
  clearTimeout(entry.timer);
  entry.resolve(reply);
}

// ---- measuring the page -----------------------------------------------------------------------------------------

function isScrollable(el: Element): boolean {
  const style = getComputedStyle(el);
  return /(auto|scroll)/.test(style.overflowY) && el.scrollHeight > el.clientHeight + 1;
}

// The element the page content scrolls in: the nearest scrolling ancestor of the content, else the document.
function scroller(sample: Element | null): Element {
  let el = sample?.parentElement ?? null;
  while (el && el !== document.body && el !== document.documentElement) {
    if (isScrollable(el)) return el;
    el = el.parentElement;
  }
  return document.scrollingElement ?? document.documentElement;
}

function visibleRect(container: Element): { top: number; bottom: number; left: number; right: number } {
  const winTop = 0;
  const winBottom = window.innerHeight;
  if (container === document.scrollingElement || container === document.documentElement) {
    return { top: winTop, bottom: winBottom, left: 0, right: window.innerWidth };
  }
  const r = container.getBoundingClientRect();
  return { top: Math.max(r.top, winTop), bottom: Math.min(r.bottom, winBottom), left: Math.max(r.left, 0), right: Math.min(r.right, window.innerWidth) };
}

// How much of `el` is on screen: full, partial or none, and the fraction of its height that shows.
function measureInView(el: HTMLElement, view: { top: number; bottom: number }): { inView: InView; fraction: number } {
  const r = el.getBoundingClientRect();
  if (r.height <= 0 || el.offsetParent === null) return { inView: "none", fraction: 0 };
  const shown = Math.max(0, Math.min(r.bottom, view.bottom) - Math.max(r.top, view.top));
  const fraction = Math.round((shown / r.height) * 100) / 100;
  if (fraction <= 0) return { inView: "none", fraction: 0 };
  return { inView: fraction >= 0.98 ? "full" : "partial", fraction };
}

function textOf(el: Element | null | undefined): string {
  return (el?.textContent ?? "").replace(/\s+/g, " ").trim();
}

// Whether an output has been drawn: it holds a picture or a table with a size, and isn't being recalculated. Nothing
// is computed to find out.
function isDrawn(output: HTMLElement | null): boolean {
  if (!output || output.classList.contains("recalculating")) return false;
  const drawn = output.querySelector("img, svg, canvas, table, .rt-table, .html-widget");
  if (!drawn) return false;
  const r = (drawn as HTMLElement).getBoundingClientRect();
  return r.width > 0 && r.height > 0;
}

// The card an output sits in, and the tab pane (if any) within that card.
function cardOf(output: HTMLElement): HTMLElement {
  return (output.closest(".cd-card") as HTMLElement | null) ?? (output.closest(".cd-plot-wrap") as HTMLElement | null) ?? output;
}

// A tabbed card's tab buttons: their ids are "<card module id>-tab_<key>" (cd_tab_strip()).
function tabButtons(card: HTMLElement): HTMLElement[] {
  const strip = card.querySelector(".cd-card__tabs");
  return strip ? (Array.from(strip.querySelectorAll(".cd-card__tab")) as HTMLElement[]) : [];
}

function tabKey(button: HTMLElement): string {
  const m = /-tab_(.+)$/.exec(button.id || "");
  return m ? m[1] : button.id || "";
}

// The longest module path the ids share ("a-b-c-plot", "a-b-d-plot" -> "a-b").
function commonPath(ids: string[]): string {
  const parts = ids.map((id) => id.split("-"));
  const out: string[] = [];
  for (let i = 0; i < parts[0].length; i++) {
    const p = parts[0][i];
    if (parts.every((x) => x[i] === p)) out.push(p);
    else break;
  }
  return out.join("-");
}

// A card's id: its module id -- from its tab buttons ("<card>-tab_<key>"), else the module path its components share
// (one component: its path without the output's own last part).
function cardIdOf(card: HTMLElement, componentIds: string[]): string {
  const buttons = tabButtons(card);
  if (buttons.length && buttons[0].id) {
    const m = /^(.*)-tab_.+$/.exec(buttons[0].id);
    if (m) return m[1];
  }
  if (componentIds.length > 1) {
    const shared = commonPath(componentIds);
    if (shared) return shared;
  }
  const one = componentIds[0] ?? card.id ?? "card";
  return one.includes("-") ? one.slice(0, one.lastIndexOf("-")) : one;
}

// A card's tab panes (cd_tab_panes()): some cards switch them with tab buttons, others follow a filter on the page.
function tabPanes(card: HTMLElement): HTMLElement[] {
  return (Array.from(card.querySelectorAll(".cd-tabpane[data-tab-key]")) as HTMLElement[]).filter((p) => p.closest(".cd-card") === card);
}

function activeTabOf(card: HTMLElement): string | undefined {
  const pane = tabPanes(card).find((p) => p.classList.contains("cd-tabpane--active"));
  if (pane?.dataset.tabKey) return pane.dataset.tabKey;
  const button = tabButtons(card).find((b) => b.getAttribute("aria-selected") === "true" || b.classList.contains("cd-card__tab--active"));
  return button ? tabKey(button) : undefined;
}

interface Measured {
  state: Record<string, unknown>;
  outputs: Map<string, HTMLElement>;
  cards: Map<string, HTMLElement>;
}

function measure(): Measured | null {
  if (!rState) return null;
  const components = rState.components ?? [];
  const outputs = new Map<string, HTMLElement>();
  for (const c of components) {
    const el = document.getElementById(c.outputId);
    if (el) outputs.set(c.id, el);
  }

  const first = outputs.values().next().value ?? document.querySelector(".cd-card");
  const container = scroller(first ?? null);
  const view = visibleRect(container);
  const isDoc = container === document.scrollingElement || container === document.documentElement;

  // group the components by the card they sit in, keeping page order
  const groups = new Map<HTMLElement, RComponent[]>();
  for (const c of components) {
    const output = outputs.get(c.id);
    if (!output) continue;
    const cardEl = cardOf(output);
    if (!groups.has(cardEl)) groups.set(cardEl, []);
    groups.get(cardEl)!.push(c);
  }
  const cardOrder = Array.from(groups.keys()).sort((a, b) => (a.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1));

  const cardEls = new Map<string, HTMLElement>();
  const cards: Card[] = [];
  const measuredComponents: Component[] = [];

  for (const cardEl of cardOrder) {
    const members = groups.get(cardEl)!;
    const cardId = cardIdOf(cardEl, members.map((c) => c.id));
    cardEl.setAttribute("data-card-id", cardId);
    const buttons = tabButtons(cardEl);
    const panes = tabPanes(cardEl);
    const { inView, fraction } = measureInView(cardEl, view);
    const tabs: Tab[] = buttons.length
      ? buttons.map((b) => ({ key: tabKey(b), label: textOf(b) }))
      : panes.map((p) => ({ key: p.dataset.tabKey as string, label: p.dataset.tabLabel || (p.dataset.tabKey as string) }));
    const card: Card = { id: cardId, title: textOf(cardEl.querySelector(".cd-card__title")), inView, visibleFraction: fraction, tabs };
    const active = tabs.length ? activeTabOf(cardEl) : undefined;
    if (active) card.activeTab = active;
    cards.push(card);
    cardEls.set(cardId, cardEl);

    for (const c of members) {
      const output = outputs.get(c.id)!;
      const pane = output.closest(".cd-tabpane") as HTMLElement | null;
      const key = pane && cardEl.contains(pane) ? pane.dataset.tabKey : undefined;
      const tab = key ? tabs.find((t) => t.key === key) : undefined;
      if (tab && !tab.componentId) tab.componentId = c.id;
      const component: Component = {
        id: c.id,
        cardId,
        type: c.type,
        title: (tab && tab.label !== tab.key ? tab.label : "") || card.title || c.id,
        drawn: isDrawn(output),
      };
      if (key) component.tabKey = key;
      if (c.about !== undefined && c.about !== null) component.about = c.about;
      measuredComponents.push(component);
    }
  }

  const state: Record<string, unknown> = { ...rState, protocol: 2 };
  state.viewport = {
    scrollTop: Math.round(isDoc ? window.scrollY : container.scrollTop),
    height: Math.round(isDoc ? window.innerHeight : container.clientHeight),
    pageHeight: Math.round(container.scrollHeight),
  };
  state.cards = cards;
  state.components = measuredComponents;
  if (askedAbout) state.askedAbout = askedAbout;
  // keep the documented key order: the page-side fields after page
  const ordered: Record<string, unknown> = {};
  for (const k of ["protocol", "app", "page", "viewport", "cards", "components"]) if (k in state) ordered[k] = state[k];
  for (const k of Object.keys(state)) if (!(k in ordered)) ordered[k] = state[k];
  return { state: ordered, outputs, cards: cardEls };
}

function getState(): unknown {
  try {
    return measure()?.state ?? null;
  } catch {
    return rState;
  }
}

// ---- actions answered by the page -------------------------------------------------------------------------------

function focusComponent(args: Record<string, unknown>): Reply {
  const id = args.componentId;
  if (typeof id !== "string" || !id) return { ok: false, error: "Say which component: componentId, from listComponents." };
  const m = measure();
  if (!m) return { ok: false, error: "The app has not said what is on the page yet." };
  const component = (m.state.components as Component[]).find((c) => c.id === id);
  if (!component) return { ok: false, error: `There is no component "${id}" on this page; listComponents gives them.` };
  const card = (m.state.cards as Card[]).find((c) => c.id === component.cardId);
  if (component.tabKey && card?.activeTab && card.activeTab !== component.tabKey) {
    const label = card.tabs.find((t) => t.key === component.tabKey)?.label ?? component.tabKey;
    return { ok: false, error: `That component is in the tab "${label}", which isn't showing; selectTab { cardId: "${card.id}", key: "${component.tabKey}" } first.` };
  }
  const el = m.cards.get(component.cardId);
  if (!el) return { ok: false, error: "The component's card is not on the page." };
  el.scrollIntoView({ block: "center" });
  return { ok: true, result: { selector: `[data-card-id="${component.cardId}"]` } };
}

function selectTab(args: Record<string, unknown>): Promise<Reply> {
  const cardId = args.cardId;
  const key = args.key;
  if (typeof cardId !== "string" || typeof key !== "string") return Promise.resolve({ ok: false, error: "Say which tab: cardId and key, from the state's cards." });
  const button = document.getElementById(`${cardId}-tab_${key}`);
  if (!button) {
    const m = measure();
    const card = (m?.state.cards as Card[] | undefined)?.find((c) => c.id === cardId);
    if (!card) return Promise.resolve({ ok: false, error: `There is no card "${cardId}" on this page.` });
    if (!card.tabs.some((t) => t.key === key)) {
      return Promise.resolve({ ok: false, error: `The card "${card.title || cardId}" has no tab "${key}". Its tabs: ${card.tabs.map((t) => t.key).join(", ") || "none"}.` });
    }
    return Promise.resolve({ ok: false, error: `The card "${card.title || cardId}" has no tab buttons: which tab it shows follows a filter on the page (e.g. the indicator); use setFilters.` });
  }
  button.click();
  const card = button.closest(".cd-card") as HTMLElement | null;
  return new Promise<Reply>((resolve) => {
    const started = Date.now();
    const check = () => {
      if ((card && activeTabOf(card) === key) || Date.now() - started > 3000) {
        resolve({ ok: true, result: getState() });
      } else {
        setTimeout(check, 100);
      }
    };
    check();
  });
}

// ---- Ask AI ------------------------------------------------------------------------------------------------------

// The user pressed an Ask AI button: `from` is the button (a card's asks about that card and the component it shows),
// or nothing for the whole page. R turns it into a prompt and asks DataSuite to open the chat with it.
export function askAi(from?: Element | null): void {
  const shiny = window.Shiny as { setInputValue?: (name: string, value: unknown, opts?: unknown) => void } | undefined;
  if (!shiny || typeof shiny.setInputValue !== "function") return;
  const at = new Date().toISOString();
  let about: AskedAbout = { scope: "page", at };
  const cardEl = (from?.closest(".cd-card") as HTMLElement | null) ?? null;
  if (cardEl) {
    let m: Measured | null = null;
    try {
      m = measure(); // also labels each card with data-card-id
    } catch {
      m = null;
    }
    const cardId = cardEl.getAttribute("data-card-id") ?? undefined;
    const card = cardId ? (m?.state.cards as Card[] | undefined)?.find((c) => c.id === cardId) : undefined;
    const components = ((m?.state.components as Component[] | undefined) ?? []).filter((c) => c.cardId === cardId);
    const shown = card?.tabs.find((t) => t.key === card.activeTab)?.componentId;
    const component = components.find((c) => c.id === shown) ?? components.find((c) => !c.tabKey || c.tabKey === card?.activeTab) ?? components[0];
    about = { scope: "card", at, title: component?.title || card?.title || textOf(cardEl.querySelector(".cd-card__title")) };
    if (cardId) about.cardId = cardId;
    if (component) about.componentId = component.id;
  }
  askedAbout = about;
  shiny.setInputValue("datasuite_ask_ai", about, { priority: "event" });
}

// A card's Ask AI button (cd_ask_ai_button()) is plain markup: one listener for them all, wherever they are drawn.
document.addEventListener("click", (event) => {
  const target = event.target as Element | null;
  const button = target && typeof target.closest === "function" ? (target.closest(".cd-card__askai") as HTMLButtonElement | null) : null;
  if (button && !button.disabled) askAi(button);
});

function toR(action: string, args: Record<string, unknown>, timeoutMs: number): Promise<Reply> {
  return new Promise<Reply>((resolve) => {
    if (!shinyConnected()) {
      resolve({ ok: false, error: "The app is not connected (its R session is not running)." });
      return;
    }
    const id = `r${Date.now().toString(36)}-${(counter++).toString(36)}`;
    const timer = setTimeout(
      () => settle(id, { ok: false, error: `The app did not answer "${action}" within ${Math.round(timeoutMs / 1000)} seconds.` }),
      timeoutMs
    );
    pending.set(id, { resolve, timer });
    try {
      window.Shiny!.setInputValue!("datasuite_ai_request", { id, action, args }, { priority: "event" });
    } catch (e) {
      settle(id, { ok: false, error: `Could not send the request: ${e instanceof Error ? e.message : String(e)}` });
    }
  });
}

window.datasuite = {
  protocol: 2,
  getState,
  request(action, args, options) {
    if (typeof action !== "string" || !action) return Promise.resolve({ ok: false, error: "No action was given." });
    const a = (args ?? {}) as Record<string, unknown>;
    try {
      switch (action) {
        case "getState": {
          const s = getState();
          return Promise.resolve(s ? { ok: true, result: s } : { ok: false, error: "The app has not said what is on the page yet." });
        }
        case "listComponents": {
          const m = measure();
          return Promise.resolve(m ? { ok: true, result: m.state.components } : { ok: false, error: "The app has not said what is on the page yet." });
        }
        case "focusComponent":
          return Promise.resolve(focusComponent(a));
        case "selectTab":
          return selectTab(a);
      }
    } catch (e) {
      return Promise.resolve({ ok: false, error: `The page could not do that: ${e instanceof Error ? e.message : String(e)}` });
    }
    return toR(action, a, options?.timeoutMs ?? 30000);
  },
};

// Shiny may finish loading after this bundle, so retry briefly instead of assuming an order (same as nav.ts).
function register(): boolean {
  const shiny = window.Shiny;
  if (!shiny || !shiny.addCustomMessageHandler) return false;
  shiny.addCustomMessageHandler("datasuite_ai_state", (message: unknown) => {
    rState = (message ?? null) as RState | null;
  });
  shiny.addCustomMessageHandler("datasuite_ai_reply", (message: unknown) => {
    const m = (message ?? {}) as { id?: string; ok?: boolean; result?: unknown; error?: string };
    if (!m.id) return;
    settle(m.id, m.ok ? { ok: true, result: m.result ?? null } : { ok: false, error: m.error || "The app could not do that." });
  });
  return true;
}
if (!register()) {
  const timer = setInterval(() => {
    if (register()) clearInterval(timer);
  }, 50);
  setTimeout(() => clearInterval(timer), 10000);
}

export {};
