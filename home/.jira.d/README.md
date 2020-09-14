https://github.com/go-jira/go-jira/issues/140


[@hagna](https://github.com/hagna) when processing the templates there is a `.meta` property which is the issue edit metadata.  That includes the `allowedValues` for properties that have restricted values.  You can see all the edit metadata with `jira editmeta ISSUE-123`.   The current `edit` templates utilize it for the `priority` field like:

```
priority: # Values: {{ range .meta.fields.priority.allowedValues }}{{.name}}, {{end}}
    name: {{ or .overrides.priority ""}}{{end}}
```

This will print a list of `# Values: Major, Minor, etc` by looping over all the allowedValues for the `priority` property.


## I want that Jira CLI Awesomeness
This script will get you all setup (assuming you cloned this project to ~/dotfiles)

```
~/dotfiles/scripts/jiraSetup
```

You should have on-hand:
- Emaill address in Jira
- Jira base URL
- Jira API token (see https://id.atlassian.com/manage/api-tokens)
- Main project name
- your "Shortname" (the name you use when you type [~first.last]) or otherwise tag yourself in Jira

The script will ask you for your information and write it to `~/dotfiles/.doNotCommit.jira`, then linking
to it from your normal `.doNotCommit` file.

Now comes the fun part. If everything is setup correctly, you can run `jira -h`

You'll see the list of default commands (`help` through `session`), and then the ones I added.

My day generally goes like this (assuming I'm on Project **ABC** with various ticket numbers)
```
jira mine # See what I've been assigned
jira s -w # s[print], Otherwise, I'll see what we have in the sprint I can snag
jira v ABC-1234 # v(iew), Look at a ticket
jira w ABC-1234 # w[orkon], Set the global issue, depends on ~/.jira.d existing
jira g # g(rab) If it wasn't mine already, grab it
jira v # v(iew), Look at it again, notice no more typing ABC-1234!
jira c -m 'This is an awesome ticket' # c(omment), Drop a comment on it
jira ts # (TransitionS), After I'm done, check where it can go next

# if I don't have  a shortcut like `d(one)` or `p(R Review)`, I use the longer syntax
jira t -s "Whatever State" -m "I've done what I can!"```

# In case I need to drop a link to a poor, non-CLI coworker - this will put it on the Mac clipboard
jira link
```

The `r(eviewed)` command is a good example of combining several actions together if you want to add your own!

The last thing I want to mention is that all of the views you see are 100% configurable; see the `.jira.d/templates` folder.

#### Jira
| Command | Params | Result |
|---------|--------|--------|
| jira w[orkon] | TicketID (e.g., PROJECT-123) | Set global story/ticket for `jira` commands |
| jira git | Branch Name | Create a new branch with JIRA_PREFIX/JIRA_ISSUE-BRANCH |
| jira s[print] | None | see the current sprint for your PROJECT |
| jira mine | None | see a list of unresolved tickets in PROJECT with you as ASSIGNEE |
| jira chrome | TicketID\* | Open ticket in Chrome |
| jira link | None | copies the link to the global ticket to the Mac clipboard |
| jira i | None | Inspect current global story/ticket for `jira` commands |
| jira v | TicketID\* | View ticket details in `bat` if available, or `cat` otherwise |
| jira e | TicketID\* | Edit(vi) |
| jira c | TicketID\*, -m | Comment(vi) on ticket, follows `-m` pattern for predefined comment |
| jira t | State, TicketID\* | Transition ticket to new state (see `jira transitions`) |
| jira d | TicketID\* | Done: Transition ticket to "Ready for QA" (feel free to modify this to be your "Dev Done" state) |
| jira g | TicketID\* | Grab: Transition ticket to "In Progress" and assigns to you (feel free to modify this to your "In Progress" state) |
| jira qa | TicketID\* | QA: Transition ticket to "Testing" and sets you as the Reviewer (feel free to modify this to your "QA" state) |
| jira r | [State], TicketID\* | Review ticket by Comment(vi) on ticket, Transition to provided state or "Signoff" by default (feel free to modify this to your preferred Post-QA stateand with your preferred review template) |
> \*NOTE: If you don't provide a TicketID, the global story/ticket set by `jira w[orkon]` is used
