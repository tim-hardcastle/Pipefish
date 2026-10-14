import "./pf-page.js";
import { attachEdgeToggle } from "./assets/edge-toggle.js";

const PfPage = customElements.get("pf-page");

const stylesheet = new URL(
    "./assets/pf-writer.css",
    import.meta.url
).href;

const MARKDOWN_ICON = `
    <svg viewBox="0 0 24 24"
         xmlns="http://www.w3.org/2000/svg"
         aria-hidden="true">
        <path
            fill="currentColor"
            stroke="none"
            d="M22.269 19.385H1.731A1.731 1.731 0 0 1 0 17.654V6.346a1.731 1.731 0 0 1 1.731-1.731h20.538A1.731 1.731 0 0 1 24 6.346v11.308a1.731 1.731 0 0 1-1.731 1.731zM5.077 16.5v-5.077l2.538 3.173 2.539-3.173V16.5h2.538V7.5h-2.538l-2.539 3.173L5.077 7.5H2.538v9h2.539zm13.846 0 3.173-4.038h-2.538V7.5h-2.539v4.962h-2.538L17.654 16.5h1.269z"
        />
    </svg>
`;

const HTML_ICON = `
    <svg viewBox="0 0 24 24"
         fill="none"
         stroke="currentColor"
         stroke-width="2"
         stroke-linecap="round"
         stroke-linejoin="round"
         aria-hidden="true">
        <path d="m8 6-6 6 6 6"/>
        <path d="m16 6 6 6-6 6"/>
        <path d="m14 4-4 16"/>
    </svg>
`;

const TOGGLE_SETTINGS = {
    size: 32,
    overlap: 16,
    inset: 17,
    viewportOffset: 12,
    editIcon: MARKDOWN_ICON,
    previewIcon: HTML_ICON
}

function loadStylesheet() {
    if (
        document.querySelector(
            `link[href="${stylesheet}"]`
        )
    ) {
        return;
    }

    const link = document.createElement("link");

    link.rel = "stylesheet";
    link.href = stylesheet;

    document.head.appendChild(link);
}

class PfWriter extends PfPage {
    constructor() {
        super();

        this.markdown = "";
        this.editing = false;
        this.loadId = 0;
        this.detachToggle = null;
    }

    async load() {
        const loadId = ++this.loadId;

        await window.pipefishReady;

        const src = this.getAttribute("src");

        if (!src) {
            return;
        }

        const response = await fetch(src);
        const markdown = await response.text();

        // Ignore an older request if src changed while fetching.
        if (loadId !== this.loadId) {
            return;
        }

        this.markdown = markdown;
        this.editing = false;

        this.renderWriter();

        this.dispatchEvent(new Event("page-loaded"));
    }

    renderWriter() {
        loadStylesheet();

        if (this.detachToggle) {
            this.detachToggle();
            this.detachToggle = null;
        }

        this.replaceChildren();

        const title = document.createElement("h1");

        title.textContent =
            this.getAttribute("title") || "";

        const rendered = document.createElement("div");

        rendered.className = "pf-writer-rendered";

        rendered.innerHTML =
            window.pipefishRenderMdAsHtml(this.markdown);

        const editor = document.createElement("textarea");

        editor.className = "pf-writer-editor";
        editor.value = this.markdown;
        editor.spellcheck = false;
        editor.setAttribute(
            "aria-label",
            "Markdown source"
        );

        editor.hidden = true;

        const toggle = document.createElement("button");
        toggle.type = "button";

        const edgeToggle = attachEdgeToggle(
            this,
            toggle,
            TOGGLE_SETTINGS
        );

        this.detachToggle = () => edgeToggle.detach();

        toggle.addEventListener("click", () => {
            if (this.editing) {
                this.markdown = editor.value;

                rendered.innerHTML =
                    window.pipefishRenderMdAsHtml(
                        this.markdown
                    );

                editor.hidden = true;
                rendered.hidden = false;

                this.editing = false;
            } else {
                editor.value = this.markdown;

                rendered.hidden = true;
                editor.hidden = false;

                this.editing = true;

                editor.focus();
            }

            edgeToggle.setMode(this.editing);
        });

        this.append(
            title,
            rendered,
            editor,
            toggle
        );

        edgeToggle.setMode(false);
    }

    disconnectedCallback() {
        if (this.detachToggle) {
            this.detachToggle();
            this.detachToggle = null;
        }
    }
}

customElements.define("pf-writer", PfWriter);