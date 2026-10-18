// The base visual element that goes inside the `pf-editor` which goes inside
// the `pf-coder` which goes inside the `pf-ide`, etc.

class PipefishReader extends HTMLElement {
    constructor() {
        super();

        this.highlighter =
            document.createElement("pf-highlighter");

        this.ready = this.highlighter.ready;

        this.files = [];
        this.currentFile = null;
        this.source = "";
        this.base = "";

        const shadow =
            this.attachShadow({ mode: "open" });

        const style =
            document.createElement("link");
        style.rel = "stylesheet";
        style.href =
            new URL("./assets/pf-reader.css", import.meta.url);

        const syntax =
            document.createElement("link");
        syntax.rel = "stylesheet";
        syntax.href =
            new URL("./assets/syntax.css", import.meta.url);

        const tabs =
            document.createElement("div");
        tabs.classList.add("tabs");

        const box =
            document.createElement("pre");
        box.classList.add("reader");

        const emptyState = document.createElement("div");
        emptyState.className = "empty-state";

        const logo = document.createElement("img");
        logo.src = new URL("./assets/rene.png", import.meta.url);
        logo.alt = "";

        emptyState.append(logo);

        shadow.append(style, syntax, tabs, box, emptyState);

        this.tabs = tabs;
        this.box = box;
        this.emptyState = emptyState
        this.fileExtractor = async (path, filename) => {
            const url = new URL(
                filename,
                new URL(
                    path,
                    new URL("book/", document.baseURI)
                )
            );

            const response = await fetch(url, {
                cache: "no-store"
            });

            if (!response.ok) {
                throw new Error(
                    `Unable to load ${url}: ` +
                    `${response.status} ${response.statusText}`
                );
            }

            return new Uint8Array(await response.arrayBuffer());
        };
    }

    async loadFiles(source) {
        const lines =
            source.replace(/\r\n/g, "\n").split("\n");

        if (
            lines.length === 0 ||
            !lines[0].trim().startsWith("./")
        ) {
            return [
                {
                    path: "main.pf",
                    data: new TextEncoder().encode(
                        normalizeSource(source)
                    )
                }
            ];
        }

        this.base = lines[0].trim();

        const rootFiles =
            this.getRootFiles(lines.slice(1));

        const files = [];

        for (const name of rootFiles) {
            const data = await this.fileExtractor(
                this.base,
                name
            );

            files.push({
                path: name,
                data
            });
        }

        return files;
    }

    connectedCallback() {
        this.initialize(this.textContent);
    }

    initialize(source) {
        this.initialization = (async () => {
            this.source = source;
        })();
        return this.initialization;
    }

    async build(fileExtractor) {
        await this.initialization
        if (fileExtractor) {
            this.fileExtractor = fileExtractor
        }
        const files = await this.loadFiles(this.source);
        this.files = files;

        if (!files.length) {
            this.currentFile = null;
            await this.display("");
            return "";
        }

        this.currentFile = files[0].path;
        this.makeTabs();
        await this.display(files[0].data);

        return files[0].data;
    }

    getRootFiles(lines) {
        let rootIndent = Infinity;

        for (const line of lines) {
            if (!line.trim()) {
                continue;
            }

            const match = line.match(/^[ \t]*/);
            const indent =
                match[0].replace(/\t/g, "    ").length;

            rootIndent =
                Math.min(rootIndent, indent);
        }

        if (rootIndent === Infinity) {
            return [];
        }

        const files = [];

        for (const line of lines) {
            if (!line.trim()) {
                continue;
            }

            const match = line.match(/^([ \t]*)(.*)$/);
            const indent =
                match[1].replace(/\t/g, "    ").length;

            if (indent !== rootIndent) {
                continue;
            }

            const name = match[2].trim();

            if (!name.endsWith("/")) {
                files.push(name);
            }
        }

        return files;
    }

    makeTabs() {
        this.tabs.innerHTML = "";

        for (const file of this.files) {
            const tab = document.createElement("button");
            tab.classList.add("tab");
            tab.dataset.path = file.path;
            if (file.path === this.currentFile) {
                tab.classList.add("selected");
            }

            const name = document.createElement("span");
            name.className = "tab-name";
            name.textContent = file.path.split("/").pop();

            const close = document.createElement("span");
            close.className = "tab-close";
            close.textContent = "×";
            close.title = "Close tab";
            close.setAttribute("role", "button");
            close.setAttribute("aria-label", `Close ${name.textContent}`);

            close.addEventListener("click", event => {
                event.stopPropagation();
                this.closeTab(file.path);
            });

            tab.append(name, close);

            tab.addEventListener("click", () => {
                this.selectFile(file.path);
            });

            this.tabs.append(tab);
        }
        this.updateEmptyState();
        this.dispatchEvent(new Event("tabschange"));
    }

    readerClear() {
        this.box.textContent = "";
        this.box.scrollTop = 0;
        this.box.scrollLeft = 0;
    }

    updateEmptyState() {
        const empty = this.files.length === 0;

        this.emptyState.style.display = empty ? "flex" : "none";
        this.box.style.opacity = empty ? "0.4" : "";
        this.tabs.style.opacity = empty ? "0.4" : "";

        if (empty) {
            this.readerClear();
            this.box.style.overflow = "hidden";
            this.currentFile = null;
        } else {
            this.box.style.overflow = "";
        }
    }

    async selectFile(path) {
        const file =
            this.files.find(file => file.path === path);

        if (!file) {
            return;
        }

        this.currentFile = path;

        this.tabs.querySelectorAll(".tab").forEach(tab => {
            tab.classList.toggle("selected", tab.dataset.path === path);
        });

        await this.display(file.data);

        this.dispatchEvent(new CustomEvent("filechange", {
            detail: {
                path: file.path,
                data: file.data
            }
        }));
    }

    async display(data) {
        await this.ready;

        if (!this.currentFile) {
            this.readerClear();
            return;
        }

        const extension =
            this.currentFile
                .slice(this.currentFile.lastIndexOf("."))
                .toLowerCase();

        const highlighters = {
            ".pf": async bytes =>
                this.highlighter.highlight(
                    new TextDecoder("utf-8").decode(bytes)
                )
        };

        const highlighter =
            highlighters[extension] ||
            (async bytes =>
                new TextDecoder("utf-8").decode(bytes));

        this.box.innerHTML =
            await highlighter(data) +
            (data.length &&
            new TextDecoder("utf-8").decode(data).endsWith("\n")
                ? "\u00a0"
                : "");
    }

    async closeTab(path) {
        const index = this.files.findIndex(file => file.path === path);
        if (index === -1) return;

        this.files.splice(index, 1);

        if (this.currentFile === path) {
            const next = this.files[Math.min(index, this.files.length - 1)];

            this.currentFile = next?.path ?? null;
            this.makeTabs();

            if (next) {
                await this.display(next.data);
                this.dispatchEvent(new CustomEvent("filechange", {
                    detail: { path: next.path, data: next.data },
                }));
            } else {
                this.makeTabs();
                this.readerClear();
            }
        } else {
            this.makeTabs();
        }
    }

    hideScrollbar() {
        this.box.style.scrollbarWidth = "none";
    }

    get scrollTop() {
        return this.box.scrollTop;
    }

    set scrollTop(value) {
        this.box.scrollTop = value;
    }

    get scrollLeft() {
        return this.box.scrollLeft;
    }

    set scrollLeft(value) {
        this.box.scrollLeft = value;
    }

    async openPaths(paths) {
        
        paths = paths.map(path => path.replace(/^\.\//, ""));
        const opened = [];

        for (const path of paths) {
            const existing = this.files.find(file => file.path === path);
            if (existing) {
                opened.push(existing);
                continue;
            }
            const data = await this.fileExtractor(this.base, path);
            opened.push({ path, data });
        }
        const openedPaths = new Set(paths);

        this.files = [
            ...opened,
            ...this.files.filter(file => !openedPaths.has(file.path)),
        ];

        if (!this.files.length) {
            this.currentFile = null;
            this.makeTabs();
            await this.display("");
            return;
        }

        this.currentFile = this.files[0].path;
        this.makeTabs();
        await this.display(this.files[0].data);

        this.dispatchEvent(new CustomEvent("filechange", {
            detail: {
                path: this.files[0].path,
                data: this.files[0].data,
            },
        }));
    }


    async removePaths(paths) {
        const normalize = path => path.replace(/^\.\//, "");
        const removed = new Set(paths.map(normalize));

        this.files = this.files.filter(
            file => !removed.has(normalize(file.path))
        );

        if (removed.has(normalize(this.currentFile ?? ""))) {
            const file = this.files[0] ?? null;

            this.currentFile = file?.path ?? null;
            this.makeTabs();

            if (file) {
                await this.display(file.data);
            } else {
                this.readerClear();
            }

            this.dispatchEvent(new CustomEvent("filechange", {
                detail: {
                    path: file?.path ?? null,
                    data: file?.data ?? "",
                },
            }));
        } else {
            this.makeTabs();
        }
    }

}

// This strips superfluous indentation from the source code given between
// `<pf-reader></pf-reader>` tags.

function normalizeSource(source) {
    const lines = source.split("\n");

    // Remove blank lines at the beginning and end.
    while (lines.length && !lines[0].trim()) {
        lines.shift();
    }

    while (lines.length && !lines[lines.length - 1].trim()) {
        lines.pop();
    }

    if (!lines.length) {
        return "";
    }

    // Find the common indentation of the non-blank lines.
    let indent = Infinity;

    for (const line of lines) {
        if (!line.trim()) {
            continue;
        }

        const match = line.match(/^[ \t]*/);
        indent = Math.min(indent, match[0].length);
    }

    // Remove the common indentation.
    for (let i = 0; i < lines.length; i++) {
        lines[i] = lines[i].slice(indent);
    }

    return lines.join("\n");
}

customElements.define("pf-reader", PipefishReader);