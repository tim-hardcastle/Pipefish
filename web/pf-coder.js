import "./pf-editor.js";
import "./pf-service.js";

class PipefishCoder extends HTMLElement {
    constructor() {
        super();

        const shadow =
            this.attachShadow({ mode: "open" });

        const service =
            document.createElement("pf-service");

        const editor =
            document.createElement("pf-editor");

        shadow.append(
            service,
            editor
        );

        this.service = service;
        this.editor = editor;
        this.dirty = false;

        editor.addEventListener("change", () => {
            this.dirty = true;
        });

        this.ready = Promise.all([
            service.ready,
            editor.ready
        ]);
    }

    initialize(source) {
        this.initialization = (async () => {
            await this.ready;
            await this.service.initialize(source);
            await this.editor.initialize(source);
            this.dirty = false;
        })();

        return this.initialization;
    }

    async build(fileExtractor) {
        await this.initialization;
        await this.editor.build(fileExtractor);
        await this.service.build(fileExtractor);
    }

    async compile() {
        await this.ready;
        if (!this.dirty) {
            return
        }
        await this.service.compileMain();
        this.dirty = false
    }

    async do(line) {
        await this.ready;
        await this.compile();
        return this.service.do(line);
    }

    async getFilepath() {
        await this.ready;
        await this.compile();
        return this.service.getFilepath();
    }
    
}

customElements.define(
    "pf-coder",
    PipefishCoder
);