import "./pf-reader.js";
import "../assets/files.js";
import "../assets/file-menu.js";

class PipefishEditor extends HTMLElement {
    constructor() {
        super();

        const shadow = this.attachShadow({ mode: "open" });

        const style = document.createElement("link");
        style.rel = "stylesheet";
        style.href =
            new URL("../assets/pf-editor.css", import.meta.url);

        const editor = document.createElement("div");
        editor.classList.add("editor");

        const logo = document.createElement("img");
        logo.classList.add("logo");
        logo.src = new URL("../assets/icon.ico", import.meta.url);
        logo.alt = "";

        const projectMenu = document.createElement("pf-file-menu");
        projectMenu.setAttribute("size", "28");

        projectMenu.addEventListener("menu-select", event => {
            const operation = event.detail.toLowerCase();

            this.files.setVFS(window.pipefishGetFileTree());
            this.files.open(operation);
        });

        const controls = document.createElement("div");
        controls.classList.add("top-controls");

        const minimize = document.createElement("button");
        minimize.classList.add("top-control", "minimize");
        minimize.innerHTML = `
            <svg viewBox="0 0 16 16" aria-hidden="true">
                <path d="M4 3h8M4 11l4-4 4 4"
                fill="none"
                stroke="currentColor"
                stroke-opacity=".8"
                stroke-width="1.5"
                stroke-linecap="round"
                stroke-linejoin="round"/>
            </svg>
        `;
        minimize.setAttribute("aria-label", "Minimize");

        controls.append(
            logo,
            projectMenu,
            minimize
        );

        const reader = document.createElement("pf-reader");
        const files = document.createElement("pf-files");

        const code = document.createElement("textarea");
        code.placeholder = "";
        code.classList.add("code-input");
        code.spellcheck = false;

        editor.append(reader, code);
        shadow.append(style, controls, editor, files);

        this.reader = reader;
        this.reader.hideScrollbar();
        this.code = code;
        this.ready = reader.ready;
        this.editor = editor;
        this.menu = projectMenu;
        this.files = files;
        this.minimizeButton = minimize;
        this.initialHeightSet = false;

        files.addEventListener("files-action", event => {
            this.handleFilesAction(event).catch(error => {
                console.error("File operation failed:", error);
            });
        });

        minimize.addEventListener("click", () => {
        const minimized =
            this.editor.classList.toggle("minimized");

        this.classList.toggle("minimized", minimized);

        this.minimizeButton.setAttribute(
            "aria-label",
            minimized ? "Maximize" : "Minimize"
        );

        this.dispatchEvent(
            new CustomEvent(
                minimized ? "minimize" : "maximize",
                {
                    bubbles: true,
                    composed: true
                }
            )
        );
    });

        code.addEventListener("input", async () => {
            await this.reader.display(code.value);

            const file = this.reader.files.find(
                file => file.path === this.reader.currentFile
            );

            if (file) {
                file.data = code.value;
            }

            await window.pipefishUpdateFile(
                this.reader.currentFile,
                code.value
            );

            this.dispatchEvent(
                new CustomEvent("change")
            );
        });

        this.reader.addEventListener("filechange", event => {
            this.code.value = event.detail.data;
            this.syncScroll();
            this.updateScrollbar();
        });

        this.reader.addEventListener("tabschange", () => {
            this.updateScrollbar();
        });

        code.addEventListener("scroll", () => {
            this.syncScroll();
        });

        code.addEventListener("wheel", event => {
            const atTop = code.scrollTop === 0;
            const atBottom =
                code.scrollTop + code.clientHeight >= code.scrollHeight;

            if (
                (atTop && event.deltaY < 0) ||
                (atBottom && event.deltaY > 0)
            ) {
                event.preventDefault();
                window.scrollBy({
                    top: event.deltaY,
                    behavior: "auto"
                });
            }
        }, { passive: false });

        code.addEventListener("keydown", event => {
            if (event.key === "Tab") {
                event.preventDefault();

                code.setRangeText(
                    "\t",
                    code.selectionStart,
                    code.selectionEnd,
                    "end"
                );

                code.dispatchEvent(new Event("input"));
                return;
            }

            if (this.handleDelimiter(event)) {
                return;
            }

            if (event.key !== "Enter") {
                return;
            }

            event.preventDefault();

            const start = code.selectionStart;
            const before = code.value.slice(0, start);
            const line = before.split("\n").pop();

            const indent = line.match(/^[\t ]*/)[0];
            const extraIndent =
                /(:\s*|--\s*)$/.test(line) ? "\t" : "";

            code.setRangeText(
                "\n" + indent + extraIndent,
                start,
                code.selectionEnd,
                "end"
            );

            code.dispatchEvent(new Event("input"));
        });
    }

    connectedCallback() {
        this.updateScrollbar()
    }

    updateScrollbar() {
        this.classList.toggle(
            "no-open-files",
            this.reader.files.length === 0
        );
    }

    refreshFileTree() {
        this.files.setVFS(window.pipefishGetFileTree());
    }

    checkFileOperation(result) {
        if (typeof result === "string" && result) {
            throw new Error(result);
        }
    }

    async handleFilesAction(event) {
        const {
            operation,
            selection,
            kind,
            name,
        } = event.detail;

        const selectedPath = selection?.path ?? null;

        const selectedType = selection?.node?.type;

        const refresh = () => this.refreshFileTree();

        switch (operation) {
            case "open": {
                if (selectedType === "file") {
                    await this.reader.openPaths([selectedPath]);
                } else if (selectedType === "folder") {
                    const paths = (selection.children || [])
                        .filter(child => child.type === "file")
                        .map(child =>
                            selectedPath === "."
                                ? child.name
                                : `${selectedPath}/${child.name}`
                        );

                    if (paths.length) {
                        await this.reader.openPaths(paths);
                    }
                }

                break;
            }

            case "download": {
                if (selectedType === "file") {
                    const result =
                        window.pipefishReadFile(selectedPath);

                    if (!result.ok) {
                        throw new Error(result.error);
                    }

                    this.downloadBlob(
                        new Blob([result.data], {
                            type: "application/octet-stream",
                        }),
                        selectedPath.split("/").pop()
                    );
                } else if (selectedType === "folder") {
                    const result =
                        window.pipefishZipDirectory(selectedPath);

                    if (!result.ok) {
                        throw new Error(result.error);
                    }

                    this.downloadBlob(
                        new Blob([result.data], {
                            type: "application/zip",
                        }),
                        `${selectedPath === "." ? "vfs" : selection.node.name}.zip`
                    );
                }

                break;
            }

            case "new": {
                if (!name || name.includes("/") || name.includes("\\")) {
                    throw new Error("Enter a valid file or folder name.");
                }

                const parent = selectedType === "folder"
                    ? selectedPath
                    : ".";

                const path = parent === "."
                    ? name
                    : `${parent}/${name}`;

                if (kind === "folder") {
                    this.checkFileOperation(
                        window.pipefishCreateDirectory(path)
                    );
                } else {
                    this.checkFileOperation(
                        window.pipefishUpdateFile(path, "")
                    );

                    await this.reader.openPaths([path]);
                }

                refresh();
                break;
            }

            case "rename": {
                if (
                    !name ||
                    name.includes("/") ||
                    name.includes("\\") ||
                    selectedPath === "."
                ) {
                    throw new Error("Enter a valid new name.");
                }

                const normalizedPath = selectedPath.replace(/^\.\//, "");

                const parent = normalizedPath.includes("/")
                    ? normalizedPath.slice(0, normalizedPath.lastIndexOf("/"))
                    : "";

                const newPath = parent
                    ? `${parent}/${name}`
                    : name;

                this.checkFileOperation(
                    window.pipefishRenamePath(normalizedPath, newPath)
                );

                const oldPrefix = normalizedPath + "/";
                
                const renamed = path =>
                    path === normalizedPath
                        ? newPath
                        : path.startsWith(oldPrefix)
                            ? newPath + path.slice(normalizedPath.length)
                            : path;

                for (const file of this.reader.files) {
                    file.path = renamed(file.path);
                }

                this.reader.currentFile = renamed(this.reader.currentFile);

                this.reader.makeTabs();
                if (this.reader.currentFile) {
                    await this.reader.selectFile(this.reader.currentFile);
                }

                refresh();
                break;
            }

            case "delete": {
                if (selectedType === "folder") {
                    this.checkFileOperation(
                        window.pipefishDeleteDirectory(selectedPath)
                    );
                } else {
                    this.checkFileOperation(
                        window.pipefishDeleteFile(selectedPath)
                    );
                }

                const removedPaths = [];

                const collectPaths = (node, path) => {
                    if (node.type === "file") {
                        removedPaths.push(path);
                        return;
                    }

                    for (const child of node.children || []) {
                        collectPaths(
                            child,
                            path === "."
                                ? child.name
                                : `${path}/${child.name}`
                        );
                    }
                };

                collectPaths(selection.node, selectedPath);

                await this.reader.removePaths(removedPaths);
                refresh();
                break;
            }

            case "revert": {
                if (selectedType !== "file" && selectedType !== "folder") {
                    break;
                }

                const target = selectedPath.replace(/^\.\//, "");

                this.checkFileOperation(
                    window.pipefishRevertFile(target)
                );

                const affectsPath = path => {
                    if (selectedType === "file") {
                        return path === target;
                    }

                    return target === "." ||
                        path === target ||
                        path.startsWith(target + "/");
                };

                const currentPath = this.reader.currentFile;
                const removedPaths = [];

                for (const file of this.reader.files) {
                    if (!affectsPath(file.path)) {
                        continue;
                    }

                    const result = window.pipefishReadFile(file.path);

                    if (result.ok) {
                        file.data = result.data;
                    } else {
                        removedPaths.push(file.path);
                    }
                }

                if (removedPaths.length) {
                    await this.reader.removePaths(removedPaths);
                }

                if (
                    currentPath &&
                    affectsPath(currentPath) &&
                    !removedPaths.includes(currentPath)
                ) {
                    await this.reader.selectFile(currentPath);
                }

                refresh();
                break;
            }
        }
    }

    downloadBlob(blob, filename) {
        const url = URL.createObjectURL(blob);
        const anchor = document.createElement("a");

        anchor.href = url;
        anchor.download = filename;

        document.body.append(anchor);
        anchor.click();
        anchor.remove();

        setTimeout(() => URL.revokeObjectURL(url), 1000);
    }





    get value() {
        return this.code.value;
    }

    set value(value) {
        this.code.value = value;
        this.display(value);
    }

    async initialize(source) {
        await this.ready;
        
        const firstLine = source.trim().split(/\r?\n/, 1)[0].trim();
        this.menu.projectName = (firstLine.startsWith("./")
            ? firstLine.replace(/\/+$/, "").split("/").pop()
            : "⋮"
        );

        const normalized =
            await this.reader.initialize(source);

        this.code.value = normalized;
        this.setInitialHeight();
        this.syncScroll();
    }

    async display(source) {
        await this.ready;

        this.code.value = source;
        await this.reader.display(source);
        this.setInitialHeight();
        this.syncScroll();
    }

    setInitialHeight() {
        if (this.initialHeightSet) {
            return;
        }

        const lines =
            Math.max(1, this.code.value.split("\n").length);

        const visibleLines =
            Math.min(lines, 20);

        const style =
            getComputedStyle(this.code);

        const lineHeight =
            parseFloat(style.lineHeight);

        const padding =
            parseFloat(style.paddingTop) +
            parseFloat(style.paddingBottom);

        const editorStyle =
            getComputedStyle(this.editor);

        const borders =
            parseFloat(editorStyle.borderTopWidth) +
            parseFloat(editorStyle.borderBottomWidth);

        const codeStyle = getComputedStyle(this.code);

        const top = parseFloat(codeStyle.top);

        this.editor.style.height =
            `${visibleLines * lineHeight + padding + borders + top}px`; 

        this.initialHeightSet = true;
    }

    syncScroll() {
        this.reader.scrollTop = this.code.scrollTop;
        this.reader.scrollLeft = this.code.scrollLeft;
    }

    isEscaped(text, pos) {
        let backslashes = 0;

        for (
            let i = pos - 1;
            i >= 0 && text[i] === "\\";
            i--
        ) {
            backslashes++;
        }

        return backslashes % 2 === 1;
    }

    quoteContext(text, pos, quote) {
        let currentQuote = null;
        let escaped = false;

        for (let i = 0; i < pos; i++) {
            const char = text[i];

            if (escaped) {
                escaped = false;
                continue;
            }

            if (char === "\\") {
                escaped = true;
                continue;
            }

            if (currentQuote === null) {
                if (
                    char === '"' ||
                    char === "'" ||
                    char === "`"
                ) {
                    currentQuote = char;
                }
            } else if (char === currentQuote) {
                currentQuote = null;
            }
        }

        return currentQuote === quote;
    }

    handleDelimiter(event) {
        const element = this.code;

        const pairs = {
            "(": ")",
            "[": "]",
            "{": "}",
            '"': '"',
            "'": "'",
            "`": "`"
        };

        const closingDelimiters = new Set([
            ")",
            "]",
            "}",
            '"',
            "'",
            "`"
        ]);

        const isQuote =
            event.key === '"' ||
            event.key === "'" ||
            event.key === "`";

        if (closingDelimiters.has(event.key)) {
            const pos = element.selectionStart;

            if (
                pos === element.selectionEnd &&
                element.value[pos] === event.key
            ) {
                if (!isQuote) {
                    event.preventDefault();
                    element.setSelectionRange(pos + 1, pos + 1);
                    return true;
                }

                if (
                    this.quoteContext(
                        element.value,
                        pos,
                        event.key
                    ) &&
                    !this.isEscaped(element.value, pos)
                ) {
                    event.preventDefault();
                    element.setSelectionRange(pos + 1, pos + 1);
                    return true;
                }
            }
        }

        if (pairs[event.key] !== undefined) {
            const start = element.selectionStart;
            const end = element.selectionEnd;

            if (
                isQuote &&
                this.quoteContext(
                    element.value,
                    start,
                    event.key
                )
            ) {
                return false;
            }

            event.preventDefault();

            const selected =
                element.value.slice(start, end);

            element.setRangeText(
                event.key + selected + pairs[event.key],
                start,
                end,
                "select"
            );

            element.setSelectionRange(
                start + 1,
                start + 1 + selected.length
            );

            element.dispatchEvent(new Event("input"));
            return true;
        }

        return false;
    }
}

customElements.define("pf-editor", PipefishEditor);
