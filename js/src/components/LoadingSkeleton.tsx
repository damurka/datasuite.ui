import React from "react";
import { tr, useLang } from "../lang";
import type { LocalText } from "../lang";

// The custom-code replacement for shinycssloaders::withSpinner()'s default spinner -- explicit user request
// ("replace all withSpinner ... use react component"), matching project/Patterns.dc.html's own "Empty and
// loading" pattern card (its second example, the bar-skeleton one) exactly. Bar count/width/heights are that
// mockup's own fixed set (confirmed by reading its raw markup), not randomized or content-derived -- this is
// a generic "something is loading" placeholder shown for whatever's underneath (a chart, a table, a plain
// uiOutput()), not a literal preview of the real chart's shape.
const BAR_HEIGHTS = [70, 110, 90, 140, 120, 160, 100];
// A table's skeleton: a header row and body rows of cells (their widths as a table's first column, then
// numbers), the same surface colour and shimmer as the bars.
const TABLE_COLUMNS = [34, 16, 16, 16, 18];
const TABLE_ROWS = 6;

export interface LoadingSkeletonProps {
  /** Always supplied translated from R (cd_text()) -- no hardcoded English fallback here, same convention
   *  every other text-carrying component in this app follows. */
  label: LocalText;
  /** What is loading: a chart (bars, the default) or a table (rows). */
  variant?: "chart" | "table";
}

function LoadingSkeleton({ label, variant = "chart" }: LoadingSkeletonProps) {
  const lang = useLang();
  return (
    <div className={variant === "table" ? "cd-skeleton cd-skeleton--table" : "cd-skeleton"} role="status" aria-live="polite">
      {variant === "table" ? (
        <div className="cd-skeleton__table" aria-hidden="true">
          {Array.from({ length: TABLE_ROWS + 1 }, (_, r) => (
            <div key={r} className={r === 0 ? "cd-skeleton__row cd-skeleton__row--head" : "cd-skeleton__row"}>
              {TABLE_COLUMNS.map((w, c) => (
                <span key={c} className="cd-skeleton__cell" style={{ flexBasis: `${w}%`, maxWidth: c === 0 ? `${w - 8 + ((r * 7) % 10)}%` : `${w - 6}%` }} />
              ))}
            </div>
          ))}
        </div>
      ) : (
        <div className="cd-skeleton__bars">
          {BAR_HEIGHTS.map((h, i) => (
            <span key={i} className="cd-skeleton__bar" style={{ height: h }} />
          ))}
        </div>
      )}
      <div className="cd-skeleton__caption">{tr(label, lang)}</div>
    </div>
  );
}

export default LoadingSkeleton;
