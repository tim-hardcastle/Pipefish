import { PfBook, parseContents } from "./pf-book.js";
import "./pf-writer.js";
import "./assets/file-menu.js";
import { FileManager } from "./assets/file-manager.js";
import { AuthorVFS } from "./assets/author-vfs.js";
import { loadWasm } from "./assets/pf-wasm.js";

class PfAuthor extends PfBook {
    constructor() {
        super();

        this.vfs = new AuthorVFS();
        this.fileManager = null;
    }

    get pageElementName() {
        return "pf-writer";
    }

    async connectedCallback() {
        await loadWasm();
        await this.vfs.initialize();
        this.vfs.openBook("using-publisher");
        await super.connectedCallback();
    }

    async loadText(src) {
        const result = window.pipefishReadBookFile("Contents.md");

        if (!result.ok) {
            throw new Error(result.error);
        }

        return result.data;
    }

    pageSrc(page) {
        return `bookfs:${page.filename}`;
    }

    async openBook(path) {
        this.vfs.openBook(path);

        const result = window.pipefishReadBookFile("Contents.md");

        if (!result.ok) {
            throw new Error(result.error);
        }

        const parsed = parseContents(result.data);

        this.header = parsed.header;
        this.contents = parsed.sections;

        this.render();

        const firstPage = this.contents[0]?.pages[0];

        if (firstPage) {
            await this.showPage(firstPage.hash);
        }
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
                "Publish book",
                "Download book"
            ],
            size
        });

        const files = document.createElement("pf-files");
        this.appendChild(files);

        this.fileManager = new FileManager(
            files,
            {
                getTree: () => this.vfs.getTree(),
                readFile: path => this.vfs.readFile(path),
                writeFile: (path, data) => this.vfs.writeFile(path, data),
                createDirectory: path => this.vfs.createDirectory(path),
                renamePath: (oldPath, newPath) =>
                    this.vfs.renamePath(oldPath, newPath),
                deleteFile: path => this.vfs.deleteFile(path),
                deleteDirectory: path => this.vfs.deleteDirectory(path),
                revertFile: path => this.vfs.revertFile(path),
                zipDirectory: path => this.vfs.zipDirectory(path),
                openBook: path => this.openBook(path)
            },
            {
                openPaths: paths => {
                    for (const path of paths) {
                        const result = this.vfs.readFile(path);

                        if (result.ok) {
                            this.dispatchEvent(new CustomEvent("file-open", {
                                detail: {
                                    path,
                                    data: result.data
                                },
                                bubbles: true,
                                composed: true
                            }));
                        }
                    }
                },
                openBook: path => this.openBook(path)
            }
        );

        filesMenu.addEventListener("menu-select", event => {
            this.fileManager.open(event.detail.toLowerCase());
        });

        booksMenu.addEventListener("menu-select", async event => {
            switch (event.detail) {
                case "Open folder as book":
                    this.fileManager.open("open-book");
                    break;

                case "Publish book":
                    break;

                case "Download book":
                    break;
            }
        });

        controls.append(filesMenu, booksMenu, themeToggle);
        header.appendChild(controls);
    }
}

customElements.define("pf-author", PfAuthor);