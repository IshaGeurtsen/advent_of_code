from string import Template
imports = ["sys", "argparse", "logging", "contextlib", "pathlib", "parselib", "typing", "traceback"]

import_template = Template("import ${package}")

file_template = Template(
"""
${imports}

class Parser(parselib.Parser[None]):
    pass


def main(args: argparse.Namespace) -> None | int | str:
    try:
        if args.debug:
            logging.basicConfig(level=logging.DEBUG)
        with contextlib.ExitStack() as stack:
            parser = parselib.FileParser(args.input, stack, Parser)
            games = parser()
            result: typing.Any = None
            sys.stdout.write("part 1: {result!s}\\n".format(result=result))
            result = None
            sys.stdout.write("part 2: {result!s}\\n".format(result=result))
            sys.stdout.flush()
    except Exception:
        if args.debug:
            raise
        return traceback.format_exc()
    else:
        return None


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("script_name")
    parser.add_argument("input", type=pathlib.Path)
    parser.add_argument("--debug", action="store_true", default=False)
    args = parser.parse_args(sys.argv)
    sys.exit(main(args))
""")

def bind():
    return file_template.substitute(imports="\n".join(import_template.substitute(package=package) for package in imports)).lstrip()

if __name__ == "__main__":
    import argparse
    import pathlib
    import sys
    parser = argparse.ArgumentParser()
    parser.add_argument("script_name")
    parser.add_argument("dir", type=pathlib.Path)
    args = parser.parse_args(sys.argv)
    args.dir.mkdir()
    (args.dir / "__main__.py").write_text(bind())
    sys.exit()