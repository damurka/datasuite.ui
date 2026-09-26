# The AI bridge

How DataSuite's chat talks to a running app built on datasuite.ui: the app tells the AI where the user is (the page,
its cards and tabs, what is in view, the filters), and the AI asks the app for things (the data behind a chart, show a
tab, go to a page, set the filters). The app side lives in datasuite.ui, so every app built on it gets it; an app only
adds what is particular to it (cd2030.core adds the Countdown filters, dataset and actions). DataSuite's side is a
generic pipe that knows no app. This is the reference; the plan it serves is `countdown-analytics/docs/AI-PLAN.md`.

```
Chat ("what does this chart show?")
  -> DataSuite tool { action: "getComponentData", args: { componentId } }
     -> in the app's page (Playwright): await window.datasuite.request("getComponentData", { componentId })
        -> Shiny.setInputValue("datasuite_ai_request", { id, action, args })            datasuite.ui JS
           -> the bridge server in app_frame() runs the action                          datasuite.ui R
           <- session$sendCustomMessage("datasuite_ai_reply", { id, ok, result|error })
        <- the promise resolves with the reply
  <- tool result
```

The page can't start a conversation with DataSuite (it is sandboxed); DataSuite reads `window.datasuite.getState()`
when it needs to (e.g. when the user sends a chat message), so "which page am I on?" is answered without a tool call.

## Protocol version 2

### In the page: `window.datasuite`

Created by the datasuite.ui JS bundle (`js/src/aibridge.ts`) on every app page.

| Member | |
| --- | --- |
| `protocol` | `2` |
| `getState()` | the state (below), measured now, or `null` before the app has published anything |
| `request(action, args?, options?)` | `Promise` of a reply: `{ ok: true, result }` or `{ ok: false, error }` (a sentence for a person). Never rejects. `options.timeoutMs` defaults to 30000; a request that times out, or is made while Shiny is disconnected, resolves `{ ok: false, error }`. |

R publishes what it knows with the custom message `datasuite_ai_state` (the page, the components with the output
each draws into, what the app adds). What only the browser knows is measured by `getState()` each time it runs:
the viewport, the cards, which tab each shows, what is in view, what has been drawn, titles. So it is right even when
the user scrolled or switched a tab a moment ago.

### The state

```json
{
  "protocol": 2,
  "app": { "name": "RMNCAH", "version": "2.1.0" },
  "page": { "id": "national_coverage", "title": "National coverage", "section": "Analysis" },
  "viewport": { "scrollTop": 620, "height": 900, "pageHeight": 2400 },
  "cards": [ { "id": "national_coverage-body", "title": "National coverage", "inView": "partial", "visibleFraction": 0.4,
               "activeTab": "anc4", "tabs": [ { "key": "anc4", "label": "ANC4", "componentId": "national_coverage-body-panel-anc4-plot" } ] } ],
  "components": [ { "id": "national_coverage-body-panel-anc4-plot", "cardId": "national_coverage-body", "tabKey": "anc4",
                    "type": "chart", "title": "ANC4", "drawn": true,
                    "about": { "kind": "coverage", "options": { "indicator": "anc4", "admin_level": "national" } } } ],
  "filters": { "admin_level": "national", "years": [2019, 2023], "indicator": "anc4" },
  "dataset": { "path": "C:/data/kenya.rds", "country": "Kenya", "revision": 42 },
  "actions": [ { "name": "navigate", "kind": "change", "description": "Opens a page.", "args": { "page": "a page id from listPages" } } ],
  "updatedAt": "2026-09-26T10:00:00Z"
}
```

- `page.id` is the id the sidebar sends (`input$tabs`).
- `viewport`: the scrolling area of the page (pixels).
- `cards`: the cards holding the page's components, in page order. `inView` is `full`, `partial` or `none`, with the
  fraction of the card's height on screen; `activeTab` and `tabs` only for a tabbed card (`tabs: []` otherwise). A
  card's id is its module id (a tabbed card's tab buttons are `<cardId>-tab_<key>`).
- `components`: the charts and tables. `drawn` says the output has been drawn (nothing is computed to find out: a tab
  never opened is simply not drawn). `title` is the tab's label, else the card's title. `about` is whatever the app
  said the component is; datasuite.ui and DataSuite pass it through untouched (cd2030.core: the report kind and its
  options).
- `filters`, `dataset` and anything else come from the app (`ai_state`); keep them small, the state goes into chat.

### Actions

Every action is `kind` `"read"` (looks; changes nothing the user sees except scrolling) or `"change"` (changes what the
user sees, or adds to the dataset). DataSuite runs read actions freely and change actions under the user's setting
`datasuite.shinyApps.aiAppControl`: `"ask"` (default; the user confirms each one), `"allow"`, or `"off"`.

Built in (datasuite.ui):

| Action | Kind | Args | Result | Answered by |
| --- | --- | --- | --- | --- |
| `getState` | read | | the state | the page |
| `listPages` | read | | `[{ id, title, section, locked }]`, every page | R |
| `listComponents` | read | | the page's components (as in the state) | the page |
| `getComponentData` | read | `componentId`, `maxRows` (default 500) | `{ componentId, columns, rows, totalRows, truncated }`: the table the component shows (what its data download writes), `rows` as objects. Works for a tab that isn't showing. | R |
| `focusComponent` | read | `componentId` | scrolls its card into view; `{ selector }` for a screenshot of the card. Refused, naming the tab, when its tab isn't showing. | the page |
| `selectTab` | change | `cardId`, `key` | shows that tab; the new state | the page |
| `navigate` | change | `page` | opens the page; the new state | R |

Added by cd2030.core for the Countdown apps (all change): `setFilters { ... }` -> the new state,
`saveReport { project }` -> `{ reportId }`, `addGraph { spec }` -> `{ graphId }`,
`generateReport { preset | reportId, format }` -> `{ file }`.

DataSuite adds one of its own, done on its side: `screenshotChart` (`componentId`) = `focusComponent`, then a
screenshot of the element at `selector`.

### In R: what an app adds

`app_frame()` (and so `cd_app()`) takes:

- `ai_state`: a function `function(session)` returning a named list merged into the state (e.g. `filters`,
  `dataset`). It is called reactively: when anything it reads changes, the state is published again.
- `ai_actions`: a list of `ai_action(name, fn, kind = c("read", "change"), description, args = list())`, where
  `fn(args, session)` returns the result (any JSON-able value), or `ai_reply_when()` to answer once the browser has
  caught up, or stops with a message for the user.

Components: `cd_plot_server()` registers every chart (its `about` argument says what it is); anything else registers
with `ai_register_component(session, output_id, type, data, about)`. Nothing is computed to list a component; its
data is computed only when `getComponentData` asks.

Errors in an action come back as `{ ok: false, error: <the condition message> }`; they never reach the user's screen.

### Custom charts and reports

The report engine checks what the AI saves: `report_validate_project()` for a report, `report_validate_spec()` for a
custom chart (report kind `custom_chart`: data from the app, fixed transforms, a plot description; never code), and
draws a plot description with `report_plot_spec()` after `report_apply_transforms()`. A `custom_chart` block keeps its
description in `spec` (the bridge may send it in `options`; `report_validate_project()` moves it).

## Protocol version 1

The first version had `charts` (with `hasData`, which computed every chart's data) instead of `cards` and
`components`, and `listCharts` / `getChartData` / `focusChart` instead of `listComponents` / `getComponentData` /
`focusComponent`. It was never released.
