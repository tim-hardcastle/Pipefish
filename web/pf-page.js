import "./pf-ide.js";

class PfPage extends HTMLElement {
    static get observedAttributes() {
        return ["src", "title"];
    }

    connectedCallback() {
        if (this.getAttribute("src")) {
            this.load();
        }
    }

    attributeChangedCallback(
        name,
        oldValue,
        newValue
    ) {
        if (
            name === "src" &&
            oldValue !== newValue &&
            this.isConnected
        ) {
            this.load();
        }
    }

    async load() {
        await window.pipefishReady;

        const src = this.getAttribute("src");
        const title = this.getAttribute("title");

        const response = await fetch(src);
        const markdown = await response.text();

        this.innerHTML = window.pipefishRenderMdAsHtml(markdown);
        await this.buildEmbeddedElements()

        const h1 = document.createElement("h1");
        h1.textContent = title;

        this.prepend(h1);

        this.dispatchEvent(new Event("page-loaded"));
    }

    async buildEmbeddedElements(fileExtractor) {
        for (const element of this.children) {
            if (element.localName.startsWith("pf-")) {
                await element.build(fileExtractor);
            }
        }
    }
}

customElements.define("pf-page", PfPage);