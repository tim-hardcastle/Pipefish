import { PfBook } from "./pf-book.js";
import "./pf-writer.js";
import "./assets/file-menu.js";

class PfAuthor extends PfBook {
    get pageElementName() {
        return "pf-writer";
    }

    renderHeaderExtras(header, themeToggle) {
        const controls = document.createElement("div");
        controls.classList.add("author-header-controls");

        const size = parseFloat(
            getComputedStyle(themeToggle).height
        );

        const filesMenu = document.createElement("pf-file-menu");
        filesMenu.setAttribute("size", String(size));
        filesMenu.projectName = "Files";

        const booksMenu = document.createElement("pf-menu");

        booksMenu.configure({
            label: "Books",
            icon: `
                <svg viewBox="0 0 24 24" aria-hidden="true">
                    <path
                        d="M12 6.5C9.5 4.5 6 4 2.5 5v14
                        c3.5-1 7-.5 9.5 1.5
                        c2.5-2 6-2.5 9.5-1.5V5
                        c-3.5-1-7-.5-9.5 1.5Z"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="1.6"
                        stroke-linejoin="round"
                    />
                    <path
                        d="M12 6.5v14"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="1.6"
                    />
                </svg>
            `,
            items: [
                "Open folder as book",
                "Publish book"
            ],
            size
        });

        controls.append(filesMenu, booksMenu, themeToggle);
        header.appendChild(controls);
    }
}

customElements.define("pf-author", PfAuthor);