import React, { useEffect, useMemo, useState } from "react";
import { InputAdapter } from "@/shiny.react";
import { tr, useLang, useMountSignal } from "../lang";
import type { LocalText } from "../lang";

// The Data adjustment page's editor: the data kept for analysis (years removed everywhere, and an area's data removed
// for some or every year), and the adjustment settings -- completeness (a k-factor for each indicator group, or an
// indicator's own), outliers and missing values -- everywhere, or with a region's or district's own settings (a
// district's win over its region's, anything not set is inherited). What the page shows and how settings resolve is
// the contract in cd2030.core's adjustment (adjust_service_data(settings =)): scopes [district, region, everywhere],
// k = the first of indicator_k[ind], group_k[g] set; outliers/missing = the first of map[ind], all_<step> set.
//
// Read-only once the data is adjusted with the saved settings, until Edit; Cancel changes goes back to them. It reports
// to Shiny { event: "adjust" | "edit" | "cancel", settings?, nonce }; R answers by pushing new props (settings,
// adjusted, busy, message) with cd_update_adjustment_editor().

type K = number;
type Flags = Record<string, boolean>;

export interface AdjScope {
  group_k: Record<string, K>;
  indicator_k: Record<string, K>;
  outliers: Flags;
  missing: Flags;
}

export interface AdjArea extends AdjScope {
  area: string;
  level: "adminlevel_1" | "district";
  region: string | null;
  all_outliers: boolean | null;
  all_missing: boolean | null;
  reported: boolean;
}

export interface AdjRemoval {
  area: string;
  level: "adminlevel_1" | "district";
  region: string | null;
  years: number[];
}

export interface AdjSettings {
  removed_years: number[];
  removals: AdjRemoval[];
  everywhere: AdjScope;
  areas: AdjArea[];
}

export interface AdjIndicator {
  id: string;
  label: LocalText;
  outliers: number | null;
  missing: number | null;
}

export interface AdjGroup {
  id: string;
  label: LocalText;
  k: boolean;
  missing: boolean;
  rr: number | null;
  below: number | null;
  indicators: AdjIndicator[];
}

export interface AdjRegion {
  region: string;
  rr: number | null;
  districts: { name: string; rr: number | null }[];
}

export interface AdjEvent {
  event: "adjust" | "edit" | "cancel";
  settings?: AdjSettings;
  nonce: number;
}

export interface AdjustmentEditorProps {
  id?: string;
  inputId?: string;
  texts: Record<string, LocalText>;
  years: number[];
  groups: AdjGroup[];
  areas: AdjRegion[];
  rrYear?: number | null;
  threshold?: number;
  rrCutoff?: number;
  kChoices?: number[];
  defaults: AdjSettings;
  settings: AdjSettings;
  adjusted?: boolean;
  busy?: boolean;
  message?: { type: "success" | "error" | "info"; text: LocalText } | null;
  value?: AdjEvent | null;
  onChange?: (value: AdjEvent) => void;
}

const EMPTY: AdjSettings = { removed_years: [], removals: [], everywhere: { group_k: {}, indicator_k: {}, outliers: {}, missing: {} }, areas: [] };

const clone = <T,>(x: T): T => JSON.parse(JSON.stringify(x)) as T;
const obj = <T,>(x: unknown): Record<string, T> => (x && typeof x === "object" && !Array.isArray(x) ? (x as Record<string, T>) : {});
const arr = <T,>(x: unknown): T[] => (Array.isArray(x) ? (x as T[]) : x == null ? [] : [x as T]);

/** The settings as the component keeps them: every map an object, every list an array (R's JSON can differ). */
function normalize(s: AdjSettings | null | undefined): AdjSettings {
  const src = s || EMPTY;
  const scope = (x: Partial<AdjScope> | undefined): AdjScope => ({
    group_k: obj<K>(x?.group_k),
    indicator_k: obj<K>(x?.indicator_k),
    outliers: obj<boolean>(x?.outliers),
    missing: obj<boolean>(x?.missing)
  });
  return {
    removed_years: arr<number>(src.removed_years).map(Number),
    removals: arr<AdjRemoval>(src.removals).map((r) => ({ area: r.area, level: r.level, region: r.region ?? null, years: arr<number>(r.years).map(Number) })),
    everywhere: scope(src.everywhere),
    areas: arr<AdjArea>(src.areas).map((a) => ({
      ...scope(a),
      area: a.area,
      level: a.level,
      region: a.region ?? null,
      all_outliers: a.all_outliers ?? null,
      all_missing: a.all_missing ?? null,
      reported: !!a.reported
    }))
  };
}

const fmtK = (k: number | null | undefined) => (k == null || !Number.isFinite(k) ? "—" : String(k));
const fmtPct = (v: number | null | undefined) => (v == null || !Number.isFinite(v) ? "—" : `${Math.round(v)}%`);
const fill = (text: string, values: Record<string, string | number>) => text.replace(/\{(\w+)\}/g, (m, k: string) => (k in values ? String(values[k]) : m));

function Chevron({ open }: { open: boolean }) {
  return (
    <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true" className={open ? "cd-adj__chev cd-adj__chev--open" : "cd-adj__chev"}>
      <path d="M9 6l6 6-6 6" />
    </svg>
  );
}

function Plus() {
  return (
    <svg width="15" height="15" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" aria-hidden="true">
      <path d="M12 5v14" />
      <path d="M5 12h14" />
    </svg>
  );
}

function RateShape({ v }: { v: number | null }) {
  if (v == null) return null;
  const shape = v < 70 ? <path d="M5 9.5L9.5 1.5h-9z" fill="#c0392b" /> : v < 90 ? <path d="M5 .5L9.5 5 5 9.5.5 5z" fill="#b57f0c" /> : <circle cx="5" cy="5" r="4.5" fill="#1b7f5a" />;
  return (
    <svg width="10" height="10" viewBox="0 0 10 10" aria-hidden="true">
      {shape}
    </svg>
  );
}

function AdjustmentEditor(props: AdjustmentEditorProps) {
  const { texts, groups = [], areas = [], years: yearsProp = [], busy, message, onChange } = props;
  useMountSignal(props.id || props.inputId);
  const lang = useLang();
  const t = (key: string, values?: Record<string, string | number>) => {
    const s = tr(texts?.[key], lang) || key;
    return values ? fill(s, values) : s;
  };
  const threshold = props.threshold ?? 90;
  const kChoices = arr<number>(props.kChoices ?? [0, 0.25, 0.5, 0.75, 1]).map(Number);
  const years = arr<number>(yearsProp).map(Number);
  const saved = useMemo(() => normalize(props.settings), [JSON.stringify(props.settings)]);
  const defaults = useMemo(() => normalize(props.defaults), [JSON.stringify(props.defaults)]);
  const adjusted = !!props.adjusted;

  const [local, setLocal] = useState<AdjSettings>(saved);
  const [editing, setEditing] = useState(false);
  const [scope, setScope] = useState<string>("*");
  const [open, setOpen] = useState<Record<string, boolean>>({});
  const [dialog, setDialog] = useState<null | "remove" | "scope">(null);
  const [query, setQuery] = useState("");
  const [pick, setPick] = useState<string | null>(null);
  const [pickYears, setPickYears] = useState<number[]>([]);

  // new settings from R (saved, or adjusted with): the page starts from them again, locked if adjusted
  useEffect(() => {
    setLocal(saved);
    setEditing(false);
  }, [saved, adjusted]);

  const ro = (adjusted && !editing) || !!busy;
  const emit = (event: AdjEvent["event"], settings?: AdjSettings) => onChange?.({ event, ...(settings ? { settings } : {}), nonce: Date.now() });
  const change = (fn: (s: AdjSettings) => void) => {
    if (ro) return;
    const next = clone(local);
    fn(next);
    setLocal(next);
  };

  // ---- areas: names, kinds, regions
  const regionOf = (name: string): string | null => {
    for (const r of areas) if (r.districts.some((d) => d.name === name)) return r.region;
    return null;
  };
  const isRegion = (name: string) => areas.some((r) => r.region === name);
  const levelOf = (name: string): AdjArea["level"] => (isRegion(name) ? "adminlevel_1" : "district");
  const findArea = (s: AdjSettings, name: string) => s.areas.find((a) => a.area === name) || null;
  const current = scope === "*" ? null : findArea(local, scope);
  const inArea = !!current;
  // the scopes a value is resolved through, nearest first
  const chainFor = (s: AdjSettings, name: string | null): AdjScope[] => {
    if (!name) return [s.everywhere];
    const own = findArea(s, name);
    const reg = levelOf(name) === "district" ? regionOf(name) : null;
    const regScope = reg ? findArea(s, reg) : null;
    return [own, regScope, s.everywhere].filter((x): x is AdjScope => !!x);
  };
  const methodK = (g: AdjGroup) => defaults.everywhere.group_k[g.id] ?? 0.25;
  const kIn = (chain: AdjScope[], g: AdjGroup, ind: string): number => {
    for (const sc of chain) {
      const v = sc.indicator_k[ind] ?? (g.k ? sc.group_k[g.id] : undefined);
      if (v !== undefined && v !== null) return Number(v);
    }
    return methodK(g);
  };
  const groupKIn = (chain: AdjScope[], g: AdjGroup): number => {
    for (const sc of chain) if (sc.group_k[g.id] !== undefined && sc.group_k[g.id] !== null) return Number(sc.group_k[g.id]);
    return methodK(g);
  };
  const flagIn = (chain: AdjScope[], field: "outliers" | "missing", ind: string): boolean => {
    for (const sc of chain) {
      const v = sc[field][ind];
      if (v !== undefined && v !== null) return !!v;
      const all = (sc as Partial<AdjArea>)[field === "outliers" ? "all_outliers" : "all_missing"];
      if (all !== undefined && all !== null) return !!all;
    }
    return true;
  };
  const allIds = groups.flatMap((g) => g.indicators.map((i) => i.id));
  // an area's step set for every indicator (all_<step>) turned into one value per indicator, so one can be reset
  const materialize = (a: AdjArea, field: "outliers" | "missing") => {
    const key = field === "outliers" ? "all_outliers" : "all_missing";
    const all = a[key];
    if (all === null) return;
    for (const id of allIds) if (a[field][id] === undefined || a[field][id] === null) a[field][id] = all;
    a[key] = null;
  };
  const ownFlag = (a: AdjArea, field: "outliers" | "missing", ind: string) => {
    const v = a[field][ind];
    if (v !== undefined && v !== null) return true;
    return (field === "outliers" ? a.all_outliers : a.all_missing) !== null;
  };
  const changedIn = (a: AdjArea, g: AdjGroup, ind: string) =>
    a.indicator_k[ind] !== undefined || (g.k && a.group_k[g.id] !== undefined) || ownFlag(a, "outliers", ind) || (g.missing && ownFlag(a, "missing", ind));
  const changedEverywhere = (g: AdjGroup, ind: string) =>
    local.everywhere.indicator_k[ind] !== undefined || local.everywhere.outliers[ind] === false || (g.missing && local.everywhere.missing[ind] === false);
  const nChanged = (a: AdjArea) => groups.reduce((n, g) => n + g.indicators.filter((i) => changedIn(a, g, i.id)).length, 0);
  const kindText = (a: { level: string; region: string | null }) => (a.level === "adminlevel_1" ? t("lbl_adj_region") : a.region ? t("lbl_adj_district_in", { region: a.region }) : t("lbl_adj_district"));
  const areaSummary = (a: AdjArea) => {
    if (a.reported) return t("lbl_adj_sum_reported");
    const parts: string[] = [];
    for (const g of groups) if (g.k && a.group_k[g.id] !== undefined) parts.push(t("lbl_adj_sum_group_k", { group: tr(g.label, lang), k: fmtK(a.group_k[g.id]) }));
    const ownK = Object.keys(a.indicator_k).length;
    if (ownK) parts.push(t("lbl_adj_sum_own_k", { n: ownK }));
    const every = (field: "outliers" | "missing") =>
      groups.filter((g) => (field === "outliers" || g.missing) && g.indicators.length > 0 && g.indicators.every((i) => ownFlag(a, field, i.id) && !flagIn([a], field, i.id))).map((g) => tr(g.label, lang));
    const noOut = every("outliers");
    if (noOut.length) parts.push(t("lbl_adj_sum_no_outliers", { groups: noOut.join(", ") }));
    const noMiss = every("missing");
    if (noMiss.length) parts.push(t("lbl_adj_sum_no_missing", { groups: noMiss.join(", ") }));
    return parts.length ? `${parts.join("; ")}; ${t("lbl_adj_sum_rest")}` : t("lbl_adj_sum_as_everywhere");
  };

  // ---- edits
  const editArea = (fn: (a: AdjArea) => void) =>
    change((s) => {
      const a = findArea(s, scope);
      if (!a) return;
      fn(a);
      a.reported = false;
    });
  const setGroupK = (g: AdjGroup, v: string) => {
    if (!inArea) change((s) => void (s.everywhere.group_k[g.id] = Number(v)));
    else editArea((a) => (v === "inherit" ? delete a.group_k[g.id] : (a.group_k[g.id] = Number(v))));
  };
  const setIndK = (ind: string, v: string) => {
    if (!inArea) change((s) => (v === "inherit" ? delete s.everywhere.indicator_k[ind] : (s.everywhere.indicator_k[ind] = Number(v))));
    else editArea((a) => (v === "inherit" ? delete a.indicator_k[ind] : (a.indicator_k[ind] = Number(v))));
  };
  const effFlag = (field: "outliers" | "missing", ind: string) => flagIn(chainFor(local, inArea ? scope : null), field, ind);
  const toggleFlag = (field: "outliers" | "missing", ind: string) => {
    const now = effFlag(field, ind);
    if (!inArea) change((s) => (now ? (s.everywhere[field][ind] = false) : delete s.everywhere[field][ind]));
    else
      editArea((a) => {
        materialize(a, field);
        a[field][ind] = !now;
      });
  };
  const resetFlag = (field: "outliers" | "missing", ind: string) =>
    editArea((a) => {
      materialize(a, field);
      delete a[field][ind];
    });
  const setGroupFlags = (g: AdjGroup, field: "outliers" | "missing", on: boolean) => {
    if (!inArea) change((s) => g.indicators.forEach((i) => (on ? delete s.everywhere[field][i.id] : (s.everywhere[field][i.id] = false))));
    else
      editArea((a) => {
        materialize(a, field);
        g.indicators.forEach((i) => (a[field][i.id] = on));
      });
  };
  const keepAsReported = () =>
    change((s) => {
      const a = findArea(s, scope);
      if (!a) return;
      a.group_k = {};
      for (const g of groups) if (g.k) a.group_k[g.id] = 0;
      a.indicator_k = {};
      a.outliers = {};
      a.missing = {};
      a.all_outliers = false;
      a.all_missing = false;
      a.reported = true;
    });
  const dropScope = () => {
    change((s) => void (s.areas = s.areas.filter((a) => a.area !== scope)));
    setScope("*");
  };
  const toggleYear = (y: number) =>
    change((s) => {
      const removed = s.removed_years.includes(y);
      const next = removed ? s.removed_years.filter((x) => x !== y) : [...s.removed_years, y].sort((a, b) => a - b);
      if (next.length < years.length) s.removed_years = next;
    });

  // ---- the dialog
  const openDialog = (kind: "remove" | "scope") => {
    if (ro) return;
    setDialog(kind);
    setQuery("");
    setPick(null);
    setPickYears([]);
  };
  const q = query.trim().toLowerCase();
  const dialogAreas = useMemo(() => {
    const out: { name: string; kind: string; region: boolean }[] = [];
    for (const r of areas) {
      const regionHit = !q || r.region.toLowerCase().includes(q);
      if (regionHit) out.push({ name: r.region, kind: t("lbl_adj_region_n", { n: r.districts.length }), region: true });
      if (q) for (const d of r.districts) if (regionHit || d.name.toLowerCase().includes(q)) out.push({ name: d.name, kind: r.region, region: false });
    }
    return out.slice(0, 120);
  }, [areas, q, lang]);
  const confirmDialog = () => {
    if (!pick) return;
    if (dialog === "remove") {
      change((s) => s.removals.push({ area: pick, level: levelOf(pick), region: levelOf(pick) === "district" ? regionOf(pick) : null, years: [...pickYears].sort((a, b) => a - b) }));
    } else {
      change((s) => {
        if (!findArea(s, pick))
          s.areas.push({ area: pick, level: levelOf(pick), region: levelOf(pick) === "district" ? regionOf(pick) : null, group_k: {}, indicator_k: {}, outliers: {}, missing: {}, all_outliers: null, all_missing: null, reported: false });
      });
      setScope(pick);
    }
    setDialog(null);
  };

  // ---- the footnote
  const footnote: string[] = [];
  if (local.removed_years.length) footnote.push(t("lbl_adj_fn_years", { years: local.removed_years.join(", ") }));
  for (const r of local.removals) footnote.push(r.years.length ? t("lbl_adj_fn_removal_years", { area: r.area, years: r.years.join(", ") }) : t("lbl_adj_fn_removal_all", { area: r.area }));
  for (const a of local.areas) footnote.push(t("lbl_adj_fn_area", { area: a.area, summary: areaSummary(a) }));
  if (!footnote.length) footnote.push(t("lbl_adj_fn_default"));

  const status = ro && !busy ? "adjusted" : adjusted ? "editing" : "not";
  const chain = chainFor(local, inArea ? scope : null);
  const parentChain = inArea ? chain.slice(1) : [];

  return (
    <div className="cd-adj">
      <div className="cd-adj__main">
        {adjusted && !editing && (
          <div role="status" className="cd-adj__banner cd-adj__banner--locked">
            <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true">
              <rect x="5" y="11" width="14" height="10" rx="2" />
              <path d="M8 11V8a4 4 0 0 1 8 0v3" />
            </svg>
            <span className="cd-adj__grow">
              <b>{t("msg_adj_locked")}</b> {t("msg_adj_locked_more")}
            </span>
            <button type="button" className="cd-adj__btn cd-adj__btn--success" disabled={busy} onClick={() => (setEditing(true), emit("edit"))}>
              {t("btn_adj_edit")}
            </button>
          </div>
        )}
        {adjusted && editing && (
          <div role="status" className="cd-adj__banner cd-adj__banner--editing">
            <span className="cd-adj__grow">
              <b>{t("msg_adj_editing")}</b> {t("msg_adj_editing_more")}
            </span>
            <button type="button" className="cd-adj__btn" onClick={() => (setLocal(saved), setEditing(false), emit("cancel"))}>
              {t("btn_adj_cancel")}
            </button>
          </div>
        )}

        <section className="cd-adj__card" aria-labelledby="cd-adj-kept">
          <div className="cd-adj__head">
            <h2 id="cd-adj-kept" className="cd-adj__h2">
              {t("title_adj_kept")}
            </h2>
            <span className="cd-adj__hint">{t("lbl_adj_kept_hint")}</span>
          </div>
          <div className="cd-adj__stack">
            <span className="cd-adj__label">{t("lbl_adj_years_everywhere")}</span>
            <div role="group" aria-label={t("lbl_adj_years_everywhere")} className="cd-adj__chips">
              {years.map((y) => {
                const removed = local.removed_years.includes(y);
                return (
                  <button key={y} type="button" aria-pressed={!removed} disabled={ro} onClick={() => toggleYear(y)} className={removed ? "cd-adj__year cd-adj__year--removed" : "cd-adj__year"}>
                    <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.4" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true">
                      {removed ? (
                        <>
                          <path d="M6 6l12 12" />
                          <path d="M18 6L6 18" />
                        </>
                      ) : (
                        <path d="M5 12l5 5 9-10" />
                      )}
                    </svg>
                    <span>{y}</span>
                  </button>
                );
              })}
            </div>
          </div>
          <div className="cd-adj__stack">
            <span className="cd-adj__label">{t("lbl_adj_also_removed")}</span>
            {local.removals.map((r, i) => (
              <div key={`${r.area}-${i}`} className="cd-adj__removal">
                <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" aria-hidden="true">
                  <circle cx="12" cy="12" r="9" />
                  <path d="M8 12h8" />
                </svg>
                <span className="cd-adj__grow">{t("lbl_adj_removed_line", { area: r.area, kind: kindText(r), when: r.years.length ? r.years.join(", ") : t("lbl_adj_every_year") })}</span>
                {!ro && (
                  <button type="button" className="cd-adj__link-btn" onClick={() => change((s) => void s.removals.splice(i, 1))}>
                    {t("btn_adj_keep_it")}
                  </button>
                )}
              </div>
            ))}
            {!ro && (
              <button type="button" className="cd-adj__add" onClick={() => openDialog("remove")}>
                <Plus />
                {t("btn_adj_remove_area")}
              </button>
            )}
          </div>
        </section>

        <section className="cd-adj__card" aria-labelledby="cd-adj-settings">
          <div className="cd-adj__stack cd-adj__stack--tight">
            <h2 id="cd-adj-settings" className="cd-adj__h2">
              {t("title_adj_settings")}
            </h2>
            <p className="cd-adj__hint cd-adj__hint--wide">{t("lbl_adj_settings_hint")}</p>
          </div>
          <div role="tablist" aria-label={t("title_adj_settings")} className="cd-adj__scopes">
            {[{ key: "*", label: t("lbl_adj_everywhere"), a: null as AdjArea | null }, ...local.areas.map((a) => ({ key: a.area, label: a.area, a }))].map((x) => (
              <button key={x.key} type="button" role="tab" aria-selected={scope === x.key} onClick={() => setScope(x.key)} className={scope === x.key ? "cd-adj__scope cd-adj__scope--on" : "cd-adj__scope"}>
                <span>{x.label}</span>
                {x.a && <span className={x.a.reported ? "cd-adj__count cd-adj__count--reported" : "cd-adj__count"}>{x.a.reported ? t("lbl_adj_as_reported") : t("lbl_adj_n_changed", { n: nChanged(x.a) })}</span>}
              </button>
            ))}
            {!ro && (
              <button type="button" className="cd-adj__add cd-adj__add--pill" onClick={() => openDialog("scope")}>
                <Plus />
                {t("btn_adj_area_settings")}
              </button>
            )}
          </div>
          {current && (
            <div className="cd-adj__strip">
              <span className="cd-adj__grow">
                <b>{current.area}</b> ({kindText(current)}): {areaSummary(current)}
              </span>
              {!ro && (
                <>
                  <button type="button" className="cd-adj__btn" onClick={keepAsReported}>
                    {t("btn_adj_keep_reported")}
                  </button>
                  <button type="button" className="cd-adj__btn cd-adj__btn--danger" onClick={dropScope}>
                    {t("btn_adj_use_everywhere")}
                  </button>
                </>
              )}
            </div>
          )}

          <div className="cd-adj__groups">
            {groups.map((g) => {
              const name = tr(g.label, lang);
              const isOpen = !!open[g.id];
              const changed = g.indicators.filter((i) => (current ? changedIn(current, g, i.id) : changedEverywhere(g, i.id)));
              const labels = changed.map((i) => tr(i.label, lang)).join(", ");
              const note = changed.length
                ? current
                  ? t("lbl_adj_changed_list", { n: g.indicators.length, m: changed.length, area: current.area, list: labels })
                  : t("lbl_adj_own_list", { n: g.indicators.length, m: changed.length, list: labels })
                : current
                  ? t("lbl_adj_all_inherited", { n: g.indicators.length })
                  : t("lbl_adj_all_group", { n: g.indicators.length });
              const gOwnK = current ? current.group_k[g.id] : local.everywhere.group_k[g.id];
              const gK = current ? (gOwnK === undefined ? "inherit" : String(gOwnK)) : String(groupKIn([local.everywhere], g));
              const outOn = g.indicators.filter((i) => effFlag("outliers", i.id)).length;
              const missOn = g.indicators.filter((i) => effFlag("missing", i.id)).length;
              const n = g.indicators.length;
              return (
                <div key={g.id} className={changed.length ? "cd-adj__group cd-adj__group--changed" : "cd-adj__group"}>
                  <div className="cd-adj__ghead">
                    <button
                      type="button"
                      className="cd-adj__arrow"
                      aria-expanded={isOpen}
                      aria-controls={`cd-adj-g-${g.id}`}
                      aria-label={isOpen ? t("btn_adj_close", { group: name }) : t("btn_adj_open", { group: name })}
                      title={isOpen ? t("btn_adj_close", { group: name }) : t("btn_adj_open", { group: name })}
                      onClick={() => setOpen({ ...open, [g.id]: !isOpen })}
                    >
                      <Chevron open={isOpen} />
                    </button>
                    <div className="cd-adj__gtext">
                      <span className="cd-adj__gname">{name}</span>
                      <span className={changed.length ? "cd-adj__gnote cd-adj__gnote--changed" : "cd-adj__gnote"}>{note}</span>
                      {g.k && g.rr != null && (
                        <span className="cd-adj__evidence">
                          <RateShape v={g.rr} />
                          <span>{t("lbl_adj_reporting", { rr: fmtPct(g.rr), year: props.rrYear ?? "—", below: g.below ?? "—", threshold })}</span>
                        </span>
                      )}
                    </div>
                    {g.k ? (
                      <label className="cd-adj__klabel">
                        {t("lbl_adj_k")}
                        <select value={gK} disabled={ro} aria-label={t("lbl_adj_k_group", { group: name })} onChange={(e) => setGroupK(g, e.target.value)} className={current && gOwnK !== undefined ? "cd-adj__select cd-adj__select--own" : "cd-adj__select cd-adj__select--group"}>
                          {current && <option value="inherit">{t("lbl_adj_inherited", { k: fmtK(groupKIn(parentChain, g)) })}</option>}
                          {kChoices.map((k) => (
                            <option key={k} value={String(k)}>
                              {fmtK(k)}
                            </option>
                          ))}
                        </select>
                      </label>
                    ) : (
                      <span className="cd-adj__muted">{t("lbl_adj_outliers_only")}</span>
                    )}
                  </div>
                  {isOpen && (
                    <table id={`cd-adj-g-${g.id}`} className="cd-adj__table">
                      <thead>
                        <tr>
                          <th scope="col" className="cd-adj__th cd-adj__th--ind">
                            {t("lbl_adj_col_indicator")}
                          </th>
                          <th scope="col" className="cd-adj__th cd-adj__th--k">
                            {t("lbl_adj_col_k")}
                          </th>
                          <th scope="col" className="cd-adj__th cd-adj__th--flag">
                            <label className="cd-adj__thcheck">
                              {t("lbl_adj_col_outliers")}
                              <input
                                type="checkbox"
                                checked={outOn === n}
                                ref={(el) => {
                                  if (el) el.indeterminate = outOn > 0 && outOn < n;
                                }}
                                disabled={ro}
                                aria-label={t("lbl_adj_outliers_all", { group: name })}
                                onChange={() => setGroupFlags(g, "outliers", outOn !== n)}
                              />
                            </label>
                          </th>
                          <th scope="col" className="cd-adj__th cd-adj__th--flag">
                            {g.missing ? (
                              <label className="cd-adj__thcheck">
                                {t("lbl_adj_col_missing")}
                                <input
                                  type="checkbox"
                                  checked={missOn === n}
                                  ref={(el) => {
                                    if (el) el.indeterminate = missOn > 0 && missOn < n;
                                  }}
                                  disabled={ro}
                                  aria-label={t("lbl_adj_missing_all", { group: name })}
                                  onChange={() => setGroupFlags(g, "missing", missOn !== n)}
                                />
                              </label>
                            ) : (
                              t("lbl_adj_col_missing")
                            )}
                          </th>
                        </tr>
                      </thead>
                      <tbody>
                        {g.indicators.map((ind) => {
                          const label = tr(ind.label, lang);
                          const custom = current ? changedIn(current, g, ind.id) : changedEverywhere(g, ind.id);
                          const ownK = current ? current.indicator_k[ind.id] : local.everywhere.indicator_k[ind.id];
                          const inheritText = current
                            ? current.group_k[g.id] !== undefined
                              ? t("lbl_adj_group_here", { k: fmtK(current.group_k[g.id]) })
                              : t("lbl_adj_inherited", { k: fmtK(kIn(parentChain, g, ind.id)) })
                            : t("lbl_adj_group_k_opt", { k: fmtK(groupKIn([local.everywhere], g)) });
                          const outOwn = !!current && ownFlag(current, "outliers", ind.id);
                          const missOwn = !!current && ownFlag(current, "missing", ind.id);
                          const count = (v: number | null) => (v == null ? "—" : v === 0 ? t("lbl_adj_none") : t("lbl_adj_months", { n: v }));
                          return (
                            <tr key={ind.id} className={custom ? "cd-adj__row cd-adj__row--custom" : "cd-adj__row"}>
                              <th scope="row" className="cd-adj__rowname">
                                {label}
                                {custom && <span className="cd-adj__tag">{current ? t("lbl_adj_changed_for", { area: current.area }) : t("lbl_adj_own")}</span>}
                              </th>
                              <td className="cd-adj__td">
                                {g.k ? (
                                  <select value={ownK === undefined ? "inherit" : String(ownK)} disabled={ro} aria-label={t("lbl_adj_k_one", { indicator: label })} onChange={(e) => setIndK(ind.id, e.target.value)} className={ownK !== undefined ? "cd-adj__select cd-adj__select--own" : "cd-adj__select"}>
                                    <option value="inherit">{inheritText}</option>
                                    {kChoices.map((k) => (
                                      <option key={k} value={String(k)}>
                                        {fmtK(k)}
                                      </option>
                                    ))}
                                  </select>
                                ) : (
                                  <span className="cd-adj__muted">—</span>
                                )}
                              </td>
                              <td className="cd-adj__td">
                                <span className="cd-adj__cell">
                                  <input type="checkbox" checked={effFlag("outliers", ind.id)} disabled={ro} aria-label={t("lbl_adj_outliers_one", { indicator: label })} onChange={() => toggleFlag("outliers", ind.id)} />
                                  {outOwn && (
                                    <button type="button" className="cd-adj__reset" disabled={ro} title={t("btn_adj_reset")} aria-label={t("btn_adj_reset")} onClick={() => resetFlag("outliers", ind.id)}>
                                      ↺
                                    </button>
                                  )}
                                  <span className={ind.outliers ? "cd-adj__count-ev" : "cd-adj__count-ev cd-adj__count-ev--none"}>{count(ind.outliers)}</span>
                                </span>
                              </td>
                              <td className="cd-adj__td">
                                {g.missing ? (
                                  <span className="cd-adj__cell">
                                    <input type="checkbox" checked={effFlag("missing", ind.id)} disabled={ro} aria-label={t("lbl_adj_missing_one", { indicator: label })} onChange={() => toggleFlag("missing", ind.id)} />
                                    {missOwn && (
                                      <button type="button" className="cd-adj__reset" disabled={ro} title={t("btn_adj_reset")} aria-label={t("btn_adj_reset")} onClick={() => resetFlag("missing", ind.id)}>
                                        ↺
                                      </button>
                                    )}
                                    <span className={ind.missing ? "cd-adj__count-ev" : "cd-adj__count-ev cd-adj__count-ev--none"}>{count(ind.missing)}</span>
                                  </span>
                                ) : (
                                  <span className="cd-adj__muted cd-adj__center">—</span>
                                )}
                              </td>
                            </tr>
                          );
                        })}
                      </tbody>
                    </table>
                  )}
                </div>
              );
            })}
          </div>
          {!ro && !inArea && (
            <button type="button" className="cd-adj__btn cd-adj__btn--quiet" onClick={() => change((s) => Object.assign(s, clone(defaults)))}>
              {t("btn_adj_default")}
            </button>
          )}
        </section>
      </div>

      <aside className="cd-adj__aside" aria-labelledby="cd-adj-sum">
        <div className="cd-adj__sumhead">
          <h2 id="cd-adj-sum" className="cd-adj__h2 cd-adj__grow">
            {t("title_adj_summary")}
          </h2>
          <span className={`cd-adj__status cd-adj__status--${status}`}>{t(status === "adjusted" ? "lbl_adj_status_adjusted" : status === "editing" ? "lbl_adj_status_editing" : "lbl_adj_status_not")}</span>
        </div>
        <dl className="cd-adj__dl">
          <div>
            <dt>{t("lbl_adj_sum_years")}</dt>
            <dd>{t("lbl_adj_sum_years_value", { kept: years.filter((y) => !local.removed_years.includes(y)).length, all: years.length })}</dd>
          </div>
          <div>
            <dt>{t("lbl_adj_sum_removals")}</dt>
            <dd>{local.removals.length}</dd>
          </div>
          <div>
            <dt>{t("lbl_adj_sum_scopes")}</dt>
            <dd>{local.areas.length}</dd>
          </div>
        </dl>
        <div className="cd-adj__foot">
          <span className="cd-adj__label">{t("lbl_adj_footnote")}</span>
          {footnote.map((f, i) => (
            <span key={i} className="cd-adj__footline">
              · {f}
            </span>
          ))}
        </div>
        {adjusted && !editing ? (
          <button type="button" className="cd-adj__cta cd-adj__cta--edit" disabled={busy} onClick={() => (setEditing(true), emit("edit"))}>
            {t("btn_adj_edit")}
          </button>
        ) : (
          <button type="button" className="cd-adj__cta" disabled={busy} aria-busy={busy || undefined} onClick={() => emit("adjust", local)}>
            {busy ? t("msg_adj_busy") : adjusted ? t("btn_adj_adjust_again") : t("btn_adj_adjust")}
          </button>
        )}
        {message && tr(message.text, lang) && (
          <p role="status" className={`cd-adj__message cd-adj__message--${message.type}`}>
            {tr(message.text, lang)}
          </p>
        )}
      </aside>

      {dialog && (
        <div className="cd-adj__overlay" role="presentation" onKeyDown={(e) => e.key === "Escape" && setDialog(null)}>
          <div role="dialog" aria-modal="true" aria-labelledby="cd-adj-dlg" className="cd-adj__dialog">
            <div className="cd-adj__dlghead">
              <h2 id="cd-adj-dlg" className="cd-adj__h2 cd-adj__grow">
                {dialog === "remove" ? t("title_adj_dlg_remove") : t("title_adj_dlg_scope")}
              </h2>
              <button type="button" className="cd-adj__close" aria-label={t("btn_adj_close_dlg")} onClick={() => setDialog(null)}>
                ×
              </button>
            </div>
            <div className="cd-adj__dlgbody">
              <span className="cd-adj__label">{t("lbl_adj_where")}</span>
              <label className="cd-adj__search">
                <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" aria-hidden="true">
                  <circle cx="11" cy="11" r="7" />
                  <path d="M20 20l-3.5-3.5" />
                </svg>
                <input type="search" autoFocus placeholder={t("lbl_adj_find")} aria-label={t("lbl_adj_find")} value={query} onChange={(e) => setQuery(e.target.value)} />
              </label>
              <div role="listbox" aria-label={t("lbl_adj_where")} className="cd-adj__list">
                {dialogAreas.map((a) => (
                  <button key={`${a.region ? "r" : "d"}-${a.name}`} type="button" role="option" aria-selected={pick === a.name} onClick={() => setPick(a.name)} className={`cd-adj__option${a.region ? " cd-adj__option--region" : ""}${pick === a.name ? " cd-adj__option--on" : ""}`}>
                    <span className="cd-adj__grow">{a.name}</span>
                    <span className="cd-adj__muted">{a.kind}</span>
                  </button>
                ))}
                {!dialogAreas.length && <p className="cd-adj__empty">{t("lbl_adj_no_match")}</p>}
                {!q && <p className="cd-adj__empty">{t("lbl_adj_type_district")}</p>}
              </div>
              {dialog === "remove" && (
                <>
                  <span className="cd-adj__label">{t("lbl_adj_when")}</span>
                  <div role="group" aria-label={t("lbl_adj_when")} className="cd-adj__chips">
                    <button type="button" aria-pressed={pickYears.length === 0} onClick={() => setPickYears([])} className={pickYears.length === 0 ? "cd-adj__pick cd-adj__pick--on" : "cd-adj__pick"}>
                      {t("lbl_adj_every_year")}
                    </button>
                    {years.map((y) => {
                      const on = pickYears.includes(y);
                      return (
                        <button key={y} type="button" aria-pressed={on} onClick={() => setPickYears(on ? pickYears.filter((x) => x !== y) : [...pickYears, y])} className={on ? "cd-adj__pick cd-adj__pick--on" : "cd-adj__pick"}>
                          {y}
                        </button>
                      );
                    })}
                  </div>
                  <p className="cd-adj__note">{t("msg_adj_remove_note")}</p>
                </>
              )}
            </div>
            <div className="cd-adj__dlgfoot">
              <button type="button" className="cd-adj__btn" onClick={() => setDialog(null)}>
                {t("btn_adj_cancel_dlg")}
              </button>
              <button type="button" className="cd-adj__cta cd-adj__cta--inline" disabled={!pick} onClick={confirmDialog}>
                {dialog === "remove" ? t("btn_adj_confirm_remove") : t("btn_adj_confirm_scope")}
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}

export default InputAdapter<AdjustmentEditorProps, AdjEvent | null>(AdjustmentEditor, (value, setValue) => ({ value, onChange: setValue }));
