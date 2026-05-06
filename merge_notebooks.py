"""
Merge new cells from Ekonometria.ipynb (remote) into Ekonometria_LOCAL.ipynb,
then copy LOCAL -> Ekonometria.ipynb.
New = cells whose id exists in remote but not in LOCAL.
LOCAL cells (including edits) are always preserved as-is.
"""
import json, shutil, sys
from pathlib import Path

BASE = Path(__file__).parent
REMOTE = BASE / "Ekonometria.ipynb"
LOCAL  = BASE / "Ekonometria_LOCAL.ipynb"

with open(REMOTE, encoding="utf-8") as f:
    remote = json.load(f)
with open(LOCAL, encoding="utf-8") as f:
    local = json.load(f)

local_ids  = {c["id"] for c in local["cells"]}
remote_ids = {c["id"] for c in remote["cells"]}

new_ids = remote_ids - local_ids
new_cells = [c for c in remote["cells"] if c["id"] in new_ids]

if new_cells:
    local["cells"].extend(new_cells)
    with open(LOCAL, "w", encoding="utf-8") as f:
        json.dump(local, f, ensure_ascii=False, indent=1)
    print(f"Merged {len(new_cells)} new cell(s) from remote into LOCAL: {[c['id'] for c in new_cells]}")
else:
    print("No new remote cells to merge.")

shutil.copy(LOCAL, REMOTE)
print(f"Copied LOCAL -> Ekonometria.ipynb  ({len(local['cells'])} cells total)")
