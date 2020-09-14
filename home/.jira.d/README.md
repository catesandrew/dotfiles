https://github.com/go-jira/go-jira/issues/140


[@hagna](https://github.com/hagna) when processing the templates there is a `.meta` property which is the issue edit metadata.  That includes the `allowedValues` for properties that have restricted values.  You can see all the edit metadata with `jira editmeta ISSUE-123`.   The current `edit` templates utilize it for the `priority` field like:

```
priority: # Values: {{ range .meta.fields.priority.allowedValues }}{{.name}}, {{end}}
    name: {{ or .overrides.priority ""}}{{end}}
```

This will print a list of `# Values: Major, Minor, etc` by looping over all the allowedValues for the `priority` property.
