export class AuthorVFS {
    constructor() {
        this.files = new Map();
        this.directories = new Set([""]);
    }

    normalize(path) {
        return path.replace(/^\.\//, "").replace(/^\/+|\/+$/g, "");
    }

    getTree() {
        const root = {
            name: "",
            type: "folder",
            children: []
        };

        const nodes = new Map([["", root]]);

        for (const dir of this.directories) {
            if (!dir) continue;

            const parentPath = dir.includes("/")
                ? dir.slice(0, dir.lastIndexOf("/"))
                : "";

            const parent = nodes.get(parentPath);
            if (!parent) continue;

            const node = {
                name: dir.split("/").pop(),
                type: "folder",
                children: []
            };

            parent.children.push(node);
            nodes.set(dir, node);
        }

        for (const [path] of this.files) {
            const parentPath = path.includes("/")
                ? path.slice(0, path.lastIndexOf("/"))
                : "";

            const parent = nodes.get(parentPath);
            if (!parent) continue;

            parent.children.push({
                name: path.split("/").pop(),
                type: "file"
            });
        }

        const sort = node => {
            node.children?.sort((a, b) => {
                if (a.type !== b.type) {
                    return a.type === "folder" ? -1 : 1;
                }

                return a.name.localeCompare(b.name);
            });

            node.children?.forEach(sort);
        };

        sort(root);
        return root;
    }

    readFile(path) {
        path = this.normalize(path);

        if (!this.files.has(path)) {
            return { ok: false, error: `File not found: ${path}` };
        }

        return { ok: true, data: this.files.get(path) };
    }

    writeFile(path, data) {
        path = this.normalize(path);

        if (!path || this.files.has(path) || this.directories.has(path)) {
            return `Path already exists or is invalid: ${path}`;
        }

        const parent = path.includes("/")
            ? path.slice(0, path.lastIndexOf("/"))
            : "";

        if (!this.directories.has(parent)) {
            return `Directory not found: ${parent}`;
        }

        this.files.set(path, data);
        return true;
    }

    createDirectory(path) {
        path = this.normalize(path);

        if (!path || this.files.has(path) || this.directories.has(path)) {
            return `Path already exists or is invalid: ${path}`;
        }

        const parent = path.includes("/")
            ? path.slice(0, path.lastIndexOf("/"))
            : "";

        if (!this.directories.has(parent)) {
            return `Directory not found: ${parent}`;
        }

        this.directories.add(path);
        return true;
    }

    renamePath(oldPath, newPath) {
        oldPath = this.normalize(oldPath);
        newPath = this.normalize(newPath);

        if (
            !oldPath ||
            !newPath ||
            oldPath === newPath ||
            newPath.startsWith(oldPath + "/") ||
            this.files.has(newPath) ||
            this.directories.has(newPath)
        ) {
            return "Invalid destination path.";
        }

        const parent = newPath.includes("/")
            ? newPath.slice(0, newPath.lastIndexOf("/"))
            : "";

        if (!this.directories.has(parent)) {
            return `Directory not found: ${parent}`;
        }

        const isFile = this.files.has(oldPath);
        const isDirectory = this.directories.has(oldPath);

        if (!isFile && !isDirectory) {
            return `Path not found: ${oldPath}`;
        }

        if (isFile) {
            this.files.set(newPath, this.files.get(oldPath));
            this.files.delete(oldPath);
            return true;
        }

        const dirs = [...this.directories]
            .filter(path => path === oldPath || path.startsWith(oldPath + "/"));

        const files = [...this.files]
            .filter(([path]) => path.startsWith(oldPath + "/"));

        for (const path of dirs) {
            this.directories.delete(path);
        }

        for (const [path] of files) {
            this.files.delete(path);
        }

        for (const path of dirs) {
            this.directories.add(
                newPath + path.slice(oldPath.length)
            );
        }

        for (const [path, data] of files) {
            this.files.set(
                newPath + path.slice(oldPath.length),
                data
            );
        }

        return true;
    }

    deleteFile(path) {
        path = this.normalize(path);

        if (!this.files.delete(path)) {
            return `File not found: ${path}`;
        }

        return true;
    }

    deleteDirectory(path) {
        path = this.normalize(path);

        if (!path || !this.directories.has(path)) {
            return "Invalid directory.";
        }

        for (const file of [...this.files.keys()]) {
            if (file.startsWith(path + "/")) {
                this.files.delete(file);
            }
        }

        for (const dir of [...this.directories]) {
            if (dir === path || dir.startsWith(path + "/")) {
                this.directories.delete(dir);
            }
        }

        return true;
    }

    revertFile(path) {
        path = this.normalize(path);

        if (!path) {
            this.files.clear();
            this.directories = new Set([""]);
            return true;
        }

        if (this.files.has(path)) {
            this.files.delete(path);
            return true;
        }

        if (this.directories.has(path)) {
            return this.deleteDirectory(path);
        }

        return `Path not found: ${path}`;
    }

    zipDirectory() {
        return {
            ok: false,
            error: "ZIP downloads are not yet implemented for the author's VFS."
        };
    }
}