import "./menu.js";

class PipefishFileMenu extends HTMLElement {
    constructor() {
        super();

        this.attachShadow({ mode: "open" });

        this.menu = document.createElement("pf-menu");

        this.menu.addEventListener("menu-select", event => {
            this.dispatchEvent(
                new CustomEvent("menu-select", {
                    detail: event.detail,
                    bubbles: true,
                    composed: true
                })
            );
        });

        this.shadowRoot.append(this.menu);
    }

    connectedCallback() {
        this.menu.configure({
            label: "⋮",
            icon: `
                <svg viewBox="0 0 16 16" aria-hidden="true">
                    <path
                        d="M2.5 4.5h4l1.5 1.5h5.5v7h-11z"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="1.4"
                        stroke-linejoin="round"/>
                </svg>
            `,
            items: [
                "Open",
                "New",
                "Rename",
                "Delete",
                "Revert",
                "Download"
            ],
            size: Number(this.getAttribute("size")) || 28
        });
    }

    set projectName(value) {
        this.menu.menuName = value;
    }
}

customElements.define("pf-file-menu", PipefishFileMenu);