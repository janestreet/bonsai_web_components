from subprocess import run
from os import listdir, path
import re

src = path.dirname(path.realpath(__file__))
repo = path.join(src, "repo")

run(["git", "clone", "https://github.com/jesseweed/seti-ui.git", repo])

icons = [
    icon
    for icon in listdir(f"{repo}/icons")
    if icon.endswith(".svg") and icon != "happenings.svg"
]
icons.sort()

mappings: dict[str, tuple[str, str]] = {}
with open(f"{repo}/styles/components/icons/mapping.less", "r") as f:
    for line in f.read().splitlines():
        if line.startswith(".icon-set"):
            args = line[10:-2]
            ext, svg, color = args.split(", ")
            ext = ext.strip("\"'")
            svg = svg.strip("\"'") + ".svg"
            if svg in icons:
                mappings[ext] = (svg, color)

colors = {}
with open(f"{repo}/styles/ui-variables.less", "r") as f:
    regexp = re.compile(r"(\@[^:]+): (\#[a-f0-9]{6})")
    for line in f.read().splitlines():
        matches = regexp.match(line)
        if matches and not matches.group(1) in ["@black", "@white"]:
            colors[matches.group(1)] = matches.group(2)

# Generate icon SVGs
svg_contents = {}
svg_tag_regexp = re.compile(r"([a-zA-Z]+)=\"([^\"]+)\"")
for icon in icons:
    with open(f"{repo}/icons/{icon}", "r") as f:
        svg = f.read()
    svg = svg.replace("</svg>", "")
    start_tag_end = svg.find(">")
    svg_start_tag = svg[: start_tag_end + 1]
    svg = svg[start_tag_end + 1 :]
    tags = [
        (match.group(1), match.group(2))
        for match in svg_tag_regexp.finditer(svg_start_tag)
        if match.group(1) is not None
    ]
    if not any(key == "viewBox" for key, _ in tags):
        width = [value for key, value in tags if key == "width"][0]
        height = [value for key, value in tags if key == "height"][0]
        tags.append(("viewBox", f"0 0 {width} {height}"))
    tags = [tag for tag in tags if tag[0] not in ["id", "width", "height"]]
    svg_contents[icon] = tags, svg

svg_ml = "module Icon = struct\n  type t ="


def icon_name(icon: str) -> str:
    return icon[:-4].lower().replace("-", "_").capitalize()


for icon in icons:
    name = icon_name(icon)
    svg_ml += f"\n    | {name}"

svg_ml += """
  [@@deriving enumerate, sexp_of]
end

let svg (icon : Icon.t) =
  match icon with"""

for icon in icons:
    name = icon_name(icon)
    tags, svg = svg_contents[icon]
    tags = "; ".join(f'"{tag[0]}", "{tag[1]}"' for tag in tags)
    svg = svg.replace('"', '\\"').replace("\n", "\\n")
    svg_ml += f'\n  | {name} -> [{tags}], "{svg}"'

svg_ml += """
;;

let mapping =
  [ """

mappings_ocaml = []
for ext in mappings:
    svg, color = mappings[ext]
    name = icon_name(svg)
    if color in colors:
        color = f'`Hex "{colors[color]}"'
    else:
        color = '`Name "currentColor"'
    mappings_ocaml.append(f'"{ext}", (Icon.{name}, {color})')
svg_ml += "\n  ; ".join(mappings_ocaml)

svg_ml += """
  ]
;;
"""

with open(f"{src}/generated.ml", "w") as f:
    f.write(svg_ml)

run(["mv", "./repo/LICENSE.md", src])
run(["rm", "-rf", repo])
