import "./pf-coder.js";
import "./pf-tui.js";

class PipefishIde extends HTMLElement {
    constructor() {
        super();

        const tui =
            document.createElement("pf-tui");

        const coder =
            document.createElement("pf-coder");

        coder.addEventListener("minimize", () => {
            this.classList.add("minimized");
        });

        coder.addEventListener("maximize", () => {
            this.classList.remove("minimized");
        });

        this.coder = coder;
        this.tui = tui;

        tui.service = coder;

        const shadow =
            this.attachShadow({ mode: "open" });

        const style =
            document.createElement("link");
        style.rel = "stylesheet";
        style.href =
            new URL("../assets/pf-ide.css", import.meta.url);

        const wrapper =
            document.createElement("div");
        wrapper.classList.add("service");

        wrapper.append(
            coder,
            tui
        );

        shadow.append(
            style,
            wrapper
        );

        this.ready = Promise.all([
            coder.ready,
            tui.ready
        ]);
    }

    connectedCallback() {
        this.initialize(this.textContent);
    }

    initialize(source) {
        this.initialization = (async () => {
            await this.ready;
            await this.tui.initialize(source);
        })();

        return this.initialization;
    }

    async build(fileExtractor) {
        await this.initialization;
        await this.tui.build(fileExtractor);
    }
}

customElements.define(
    "pf-ide",
    PipefishIde
);