# envsubst

Clone of [envsubst](https://www.gnu.org/software/gettext/manual/html_node/envsubst-Invocation.html). Supports [parameter expansion](http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_06_02).

## usage

```bash
envsubst < INPUT > OUTPUT
```

## parameter expansion

The following expansions are supported:

| form | set and not null | set but null | unset |
| ${parameter:-word} | substitute parameter | substitute word | substitute word |
| ${parameter-word} | substitute parameter | substitute null | substitute word |
| ${parameter:?word} | substitute parameter | error, exit | error, exit |
| ${parameter?word} | substitute parameter | substitute null | error, exit |
