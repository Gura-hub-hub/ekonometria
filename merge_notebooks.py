#!/usr/bin/env python3
"""
Merges remote Ekonometria.ipynb into Ekonometria_LOCAL.ipynb.

Strategy (matched by cell ID):
  - Cell exists in LOCAL   → keep LOCAL's content (user's changes preserved)
  - Cell new in remote     → add it (new content from GitHub)
  - Cell only in LOCAL     → keep it at the end (user's additions)
"""

import json
import shutil
from pathlib import Path

REPO = Path(__file__).parent
REMOTE = REPO / "Ekonometria.ipynb"
LOCAL = REPO / "Ekonometria_LOCAL.ipynb"


def cell_id(cell):
    return cell.get("id") or "|".join(cell.get("source", []))[:80]


def merge():
    with open(REMOTE, encoding="utf-8") as f:
        remote_nb = json.load(f)
    with open(LOCAL, encoding="utf-8") as f:
        local_nb = json.load(f)

    remote_cells = remote_nb["cells"]
    local_cells = local_nb["cells"]

    local_by_id = {cell_id(c): c for c in local_cells}
    remote_ids = {cell_id(c) for c in remote_cells}

    merged = []
    added_new = 0
    kept_local = 0

    for rc in remote_cells:
        cid = cell_id(rc)
        if cid in local_by_id:
            merged.append(local_by_id[cid])
        else:
            merged.append(rc)
            added_new += 1

    for lc in local_cells:
        if cell_id(lc) not in remote_ids:
            merged.append(lc)
            kept_local += 1

    local_nb["cells"] = merged

    with open(LOCAL, "w", encoding="utf-8") as f:
        json.dump(local_nb, f, indent=1, ensure_ascii=False)

    shutil.copy(LOCAL, REMOTE)

    print(f"Merged: {len(merged)} cells total")
    print(f"  + {added_new} new cells from remote")
    print(f"  + {kept_local} local-only cells preserved")
    print(f"Copied LOCAL -> Ekonometria.ipynb")


if __name__ == "__main__":
    merge()
