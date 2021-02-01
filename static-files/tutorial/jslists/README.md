# jslists
A library for creating and managing dynamic collapsible lists.

When a recent project required a tree view for displaying MI data, I wanted to use a basic HTML list setup. After a quick trawl, there are either heavy weight libraries like jsTree which have a dependency on jQuery or very light weight libraries that didn't really give me what I needed.

So, I sat down and penned the following little library! It's lightweight enough right now. If I find time I will try and add the features I currently have in dev such as find, expand/close all, drag drop of list nodes.

Load using a normal script tag
```html
<script src="jsLists.min.js"></script>
```

Calling JSLists is simple. In the example below, we simply pass the ID of the list we want to collapse:
```html
<script>JSLists.applyToList('simple_list', 'ALL');</script>
```
There are two parameters in JSLists.applyToList. The first is the ID of the list list you want to collapse.
The second is the list type, so UL [Unordered list], OL [Ordered list], or ALL.
