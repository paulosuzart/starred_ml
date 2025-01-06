Starred_ml - Github Awesome list based on starred proejct
---

Are you a compulsive Github stargazer? `Starred_ml` is here for you! It will access Github `https://api.github.com/user/starred` API and fetch all starred repositories and dump a mardown that you can use as
`README.md` in a repository.

## Running


```shell
opam update
opam install starred_ml


TOKEN={{your github personal token}} starred_ml > README.md 
```

For full option list, run `starred_ml --help`.


_See [Templating](#Templating) section for details on the output markdown._

Will output something liket this:  

```mardkown

Awesome Stars
===

> A curated list of Github stars! Generated by [starred_ml](https://github.com/paulosuzart/starred_ml)


Languages
---

Total of `7` Languages starred:

   - [C](#C)
   - [C#](#C#)
   - [C++](#C++)
   - [Haskell](#Haskell)
   - [Java](#Java)
   - [Jupyter Notebook](#Jupyter-Notebook)
   - [Lua](#Lua)
 ...

Contents
---

## C
   - [pikasTech/PikaPython](https://api.github.com/repos/pikasTech/PikaPython) - An ultra-lightweight Python interpreter that runs with only 4KB of RAM, zero dependencies. It is ready to use out of the box without any configuration required and easy to extend with C. Similar project: MicroPython, JerryScript.
## C#
   - [microsoft/kiota](https://api.github.com/repos/microsoft/kiota) - OpenAPI based HTTP Client code generator
...

```

Generating your awesome `README.md`
---

You can run `starred_ml` locally, but it makes more sense that you atomatically generate a README.md in dedicated github repository. For that you can use my Awesome as a template [`https://github.com/paulosuzart/awesome`](https://github.com/paulosuzart/awesome).

You need two things:

   1. Select `Allow GitHub Actions to create and approve pull requests` under (YOUR REPO)/Settings/Actions/General.
   1. A PAT [Personal Access Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens) with starred read-only access that you must set to your awesome repository under 
   (YOUR REPO)/Settings/Secrets and variables/Actions, then in the `Repository Secrets` you can add your generated token with the name `TOKEN`. 


Templating
---

`Starred_ml` uses [jingoo](https://github.com/tategakibunko/jingoo) as a template engine. Create a template file (default is `default.jingoo`) if you want to customize. 
You can reuse the provided template [here](default.jingoo) and customize as you see fit. You can save the template with a different name. Use `--template` the specify a different template file. 

The availabe variables are:
   - `lang_count` - The total count of languages found (Including `Not set`)
   - `languages` - a list of languages detected across all starred repos. The slug of the language is provided.
   - `by_language` - a list of objects representing starred repositories with the following keys:
      - `language` - the slug language of in question
      - `starred` - a list of objects with the following keys:
         - `name` - name of the repository
         - `language_slug` - slug version of the language
         - `html_url` - Github url
         - `description` - repository description
         - `owner_login` - the repository owner login

Similar projects
---
While developing `Starred_ml` I found at least one alternative:

   - [smaguowei/starred](https://github.com/maguowei/starred) written in python. I then proceeded to use a similar output
