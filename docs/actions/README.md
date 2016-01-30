# Actions

Actions are data types which describe the user's intent to change a view's
state. Actions don't actually make changes to state - that's the job of the
`Update` function, which interprets actions by updating the state and
performing effects.

{%purs%}
data Action = AddTodo Todo | EditTodo Todo | DeleteTodo UUID
{%endpurs%}

- [Sending Actions](/SendingActions.md)
- [Updating State](/UpdatingState.md)
