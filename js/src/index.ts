import "./spinner";
import "./tabswitch";
import "./dialog";
import { askAi } from "./aibridge";
import { InputAdapter } from "@/shiny.react";
import {
  AdjustmentEditor,
  ChartCustomize,
  ChipMulti,
  ChipNumber,
  ChipSelect,
  CardHeader,
  CdButton,
  CdCheckbox,
  CdTextArea,
  DownloadButtonStatus,
  EmptyState,
  ExpandButton,
  FieldNumber,
  FieldSelect,
  FileUploadZone,
  LoadingSkeleton,
  MappingModal,
  MessageBoxStatus,
  StatusBanner,
  Tooltip,
  WizardSteps,
  HeaderBreadcrumb,
  HeaderActions,
  Sidebar,
  inputValueProps,
  setUiHost,
  shinyHost,
} from "@quire/components";

// The components come from @quire/components (datasuite-ui-kit), whose host is Shiny by default; the header's Ask AI
// button goes to the AI bridge here (aibridge.ts).
setUiHost({ ...shinyHost, askAi });

// The inputs among them, made Shiny inputs (input$<inputId>, updateReactInput()) with shiny.react.
const v = inputValueProps;

// Components are used from R as shiny.react::reactElement(module = "@/countdown", name = "<Component>", ...)
// -- see cd_react_element() in apps/rmncah/_shared/R/core (and components/).
window.jsmodule = {
  ...window.jsmodule,
  "@/countdown": {
    AdjustmentEditor: InputAdapter(AdjustmentEditor, v.AdjustmentEditor),
    ChartCustomize: InputAdapter(ChartCustomize, v.ChartCustomize),
    ChipMulti: InputAdapter(ChipMulti, v.ChipMulti),
    ChipNumber: InputAdapter(ChipNumber, v.ChipNumber),
    ChipSelect: InputAdapter(ChipSelect, v.ChipSelect),
    CardHeader,
    CdButton,
    CdCheckbox: InputAdapter(CdCheckbox, v.CdCheckbox),
    CdTextArea: InputAdapter(CdTextArea, v.CdTextArea),
    DownloadButtonStatus,
    EmptyState,
    ExpandButton,
    FieldNumber: InputAdapter(FieldNumber, v.FieldNumber),
    FieldSelect: InputAdapter(FieldSelect, v.FieldSelect),
    FileUploadZone,
    LoadingSkeleton,
    MappingModal: InputAdapter(MappingModal, v.MappingModal),
    MessageBoxStatus,
    StatusBanner,
    Tooltip,
    WizardSteps: InputAdapter(WizardSteps, v.WizardSteps),
    HeaderBreadcrumb,
    HeaderActions,
    Sidebar,
  },
};
