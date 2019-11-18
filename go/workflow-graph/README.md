# Brook

## Notes

```
Lock step
Query for step running state
If step has already been started
  Unlock step
else
  Query for workflow running state
  If Workflow is not cancelled or errored
    Record step as running
    Unlock step
    Run step
    Wait for step to finish
    If step errored
      Mark workflow as errored
      Mark step as errored
      Send alerts
    Else
      Look up children of step.
      For each child
        If all the parents of the child have completed
          Run the child in a new thread
```
