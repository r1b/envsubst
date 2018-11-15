# envsubst

Clone of [envsubst](https://www.gnu.org/software/gettext/manual/html_node/envsubst-Invocation.html). Supports [parameter expansion](http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_06_02).

## usage

```bash
envsubst < INPUT > OUTPUT
```

## semantics

I try to be true to the semantics of the original `envsubst` which are slightly
different from the semantics in a shell. These differences include:

* Most "bad substitutions" are allowed
    * `${}` -> "${}"
    * `${{foo}` -> `${{foo}`
* Nested expansions are allowed:
    * `export foo=bar ${${foo}}` -> `${bar}`
    * ...but they only expand at depth=1 ;p

Unlike `envsubst`, shell builtin variables like `$$` and `$0` are not supported.

## parameter expansion

The following expansions are supported:

| form | set and not null | set but null | unset |
| ---- | ---------------- | ------------ | ----- |
| ${parameter:-word} | substitute parameter | substitute word | substitute word |
| ${parameter-word} | substitute parameter | substitute null | substitute word |
| ${parameter:?[word]} | substitute parameter | error, exit | error, exit |
| ${parameter?[word]} | substitute parameter | substitute null | error, exit |

## testing

```bash
chicken-install -n && csi tests/run.scm
```

## todo

* Support `${parameter:+word}` and `${parameter+word}`
* Packaging
* Escapes
* Whitelisting
* Inplace
* Test `main`
* Configure strictness
* Add reference tests against envsubst

## todo if ur bored

* Add --posix
