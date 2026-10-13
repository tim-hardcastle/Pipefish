class PipefishMenu extends HTMLElement {
    constructor() {
        super();

        this.attachShadow({ mode: "open" });

        this.items = [];
        this.label = "";
        this.icon = "";
        this.size = 28;
        this.menuWidth = "8rem";
    }

    configure({
        label = "",
        icon = "",
        items = [],
        size = 28,
        menuWidth = "8rem"
    } = {}) {
        this.label = label;
        this.icon = icon;
        this.items = items;
        this.size = size;
        this.menuWidth = menuWidth;

        this.render();
    }

    set menuName(value) {
        this.label = value;
        this.render();
    }

    render() {
        const shadow = this.shadowRoot;
        shadow.replaceChildren();

        const style = document.createElement("style");
        style.textContent = `
            :host {
                position: relative;
                display: flex;
                flex: none;
                align-items: center;
                pointer-events: auto;
                font-family: "Google Sans Code", monospace;
            }

            button {
                box-sizing: border-box;
                border: 1px solid var(--code-border);
                border-radius: 7px;
                background: var(--paper);
                color: var(--code-text);
                font-family: inherit;
                cursor: pointer;
            }

            .trigger {
                height: ${this.size}px;
                padding: 0 .65rem;
                display: flex;
                align-items: center;
                gap: .4rem;
                font-size: 14px;
            }

            .trigger svg {
                width: 14px;
                height: 14px;
                flex: none;
                display: block;
            }

            .dropdown {
                position: absolute;
                z-index: 10;
                top: calc(100% - 1px);
                right: 0;

                display: none;
                flex-direction: column;

                min-width: ${this.menuWidth};
                padding: .25rem;

                border: 1px solid var(--code-border);
                border-radius: 7px;

                background: var(--paper);
                box-shadow: 0 4px 12px rgb(0 0 0 / 12%);
            }

            .dropdown.open {
                display: flex;
            }

            .item {
                width: 100%;
                padding: .45rem .65rem;
                border: 0;
                border-radius: 5px;
                background: transparent;
                color: var(--code-text);
                font-size: 13px;
                text-align: left;
                white-space: nowrap;
            }

            .item:hover {
                background: var(--code-border);
            }
        `;

        const trigger = document.createElement("button");
        trigger.type = "button";
        trigger.className = "trigger";
        trigger.setAttribute("aria-haspopup", "true");
        trigger.setAttribute("aria-expanded", "false");

        if (this.icon) {
            trigger.insertAdjacentHTML("beforeend", this.icon);
        }

        const label = document.createElement("span");
        label.textContent = this.label;
        trigger.append(label);

        const dropdown = document.createElement("div");
        dropdown.className = "dropdown";

        for (const item of this.items) {
            const button = document.createElement("button");
            button.type = "button";
            button.className = "item";
            button.textContent = item;

            button.addEventListener("click", () => {
                this.close();

                this.dispatchEvent(
                    new CustomEvent("menu-select", {
                        detail: item,
                        bubbles: true,
                        composed: true
                    })
                );
            });

            dropdown.append(button);
        }

        trigger.addEventListener("click", event => {
            event.stopPropagation();

            if (dropdown.classList.contains("open")) {
                this.close();
            } else {
                this.open();
            }
        });

        this._trigger = trigger;
        this._dropdown = dropdown;

        shadow.append(style, trigger, dropdown);
    }

    open() {
        this._dropdown?.classList.add("open");
        this._trigger?.setAttribute("aria-expanded", "true");
    }

    close() {
        this._dropdown?.classList.remove("open");
        this._trigger?.setAttribute("aria-expanded", "false");
    }

    toggle() {
        if (this._dropdown?.classList.contains("open")) {
            this.close();
        } else {
            this.open();
        }
    }
}

customElements.define("pf-menu", PipefishMenu);