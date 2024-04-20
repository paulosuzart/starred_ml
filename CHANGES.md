## 0.0.4

- Added [CmdLiner](https://erratique.ch/software/cmdliner/doc/Cmdliner/) for a smoother experience. The addition is fully compatible with previous `TOKEN` env var only.
- Moved some code for better testability, leaving `bin` for pure execution.
- Added a bit more tests

## 0.0.3

- The generated output was using `url` of the repo, that points to the GitHub API. This release fixes it by using the proper `html_url`.
- Some other small typos were fixed
- A reference to the template repository using `starred_ml` was also added

## 0.0.2

Includes pagination and total language count. See the template.

## 0.0.1

The very initial release of `Starred_ml`. This version is intended to help setting up CI but it is working already.

**IMPORTANT** This release won't use pagination on Github API, fetching only the first page of the starred items.
