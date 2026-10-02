// What the page has besides this bundle: Shiny, and shiny.react's registry of components (ours is "@/countdown",
// index.ts). The components themselves (@quire/components) do not need these: they reach Shiny through their host.

interface ShinyGlobal {
  addCustomMessageHandler?: (type: string, handler: (message: unknown) => void) => void;
  setInputValue?: (name: string, value: unknown, options?: { priority?: string }) => void;
  bindAll?: (scope?: Element) => void;
  initializeInputs?: (scope?: Element) => void;
  unbindAll?: (scope?: Element) => void;
}

interface Window {
  Shiny?: ShinyGlobal & Record<string, unknown>;
  jsmodule?: Record<string, unknown>;
}
