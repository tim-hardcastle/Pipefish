import "./pf-page.js";
import { attachEdgeToggle } from "./assets/edge-toggle.js";

const PfPage = customElements.get("pf-page");

const stylesheet = new URL(
    "./assets/pf-writer.css",
    import.meta.url
).href;

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

        toggle.className = "pf-writer-toggle";
        toggle.type = "button";
        toggle.textContent = "Edit";
        toggle.setAttribute("aria-label", "Edit Markdown");

        toggle.addEventListener("click", () => {
            if (this.editing) {
                // Return to rendered mode, using the latest source.
                this.markdown = editor.value;

                rendered.innerHTML =
                    window.pipefishRenderMdAsHtml(
                        this.markdown
                    );

                editor.hidden = true;
                rendered.hidden = false;

                toggle.textContent = "Edit";
                toggle.setAttribute(
                    "aria-label",
                    "Edit Markdown"
                );
            } else {
                // Enter editing mode.
                editor.value = this.markdown;

                rendered.hidden = true;
                editor.hidden = false;

                toggle.textContent = "Preview";
                toggle.setAttribute(
                    "aria-label",
                    "Preview rendered page"
                );

                editor.focus();
            }

            this.editing = !this.editing;
        });

        this.append(
            title,
            rendered,
            editor,
            toggle
        );

        this.detachToggle = attachEdgeToggle(
            this,
            toggle
        );
    }

    disconnectedCallback() {
        if (this.detachToggle) {
            this.detachToggle();
            this.detachToggle = null;
        }
    }
}

customElements.define("pf-writer", PfWriter);