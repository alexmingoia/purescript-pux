# Components

A component can be thought of as any module exposing its own state, event type,
foldp and view functions. That is useful for open-sourcing generic Pux
components, but is it a good way to organize your application?

If you have React or Elm experience, you're taught to think of your application
as self-contained, inter-locking components that encapsulate business logic and
presentation. But that object-oriented perspective is unnecessary when working
with a purely functional language like PureScript. Components are just one of
the many ways you could organize your Pux application, because an application
is really a single foldp and view function. They can be split into smaller
functions however best fit your needs.

## Organize code using the Pux App Architecture

Instead of designing your application around components, design it around *data
flow*: Events change state (foldp), and state changes HTML (view).  The most
basic application (a todo list) could have a simpler architecture without
components, just a foldp and view function in their own files. This is how the
Pux [TodoMVC](https://github.com/alexmingoia/pux-todomvc) example is organized.

As an application grows, it makes sense to split up the view into smaller
views. For example, an app with different pages would benefit from having a view
for each page. This is easy too, because views are "dumb" and it's obvious which
view is contained by the file name "TodoList.purs" or "TodoItem.purs".

Organizing the todo list into components with files for TodoList and TodoItem –
each with their own event and state type and foldp and view function – would
introduce boilerplate for mapping between types, and it doesn't make the code
any easier to understand. In a larger application, having components makes
reorganizing views cumbersome because you need to reorganize the foldp function
and event type contained in the same file.

Instead of components, split up the foldp function into modules that are separate
from your views and organized around business logic. You may have a module that
handles user events like logging in and out, another for todo list events, etc.
along with your top-level foldp. Views can live separately and import the
events from those other foldp modules as needed. It's easier to reorganize
business logic and views with this architecture. Those with Redux experience
will see the similarity to actions and reducers.
