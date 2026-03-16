---
id: task-022
title: make --batch flag set --noaudio as well
status: Done
assignee: []
created_date: '2025-12-19 23:03'
updated_date: '2025-12-20 04:14'
labels: []
dependencies: []
---

Because I don't _think_ that there's any scenario where you want batch with the
audio stuff running.

We can grep through the codebase to update all the calls in e.g. the cmake build
process.
