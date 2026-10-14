import { PfBook } from "./pf-book.js";
import "./pf-writer.js";

class PfAuthor extends PfBook {
    get pageElementName() {
        return "pf-writer";
    }
}

customElements.define("pf-author", PfAuthor);