# RPIA-Scheduler
## Modules
### Scheduling
The scheduling module contains the meat of this project. 
Majority of the project datatypes are defined here.

The primary role of the scheduling module is to handle the generation of schedules.
A typical workflow involving scheduling will likely call the `generateSchedules` function with
a list of crew, passing the list of generated schedules forward to additional processing.

### Heuristic
The heuristic module contains the heuristics definitions of the project.
This module holds the functions responsible for grading and checking schedules.

A typical workflow is expected to take a list of generated schedules and to call `rankSchedules` to grade
and rank the proposed schedules.

### Persistence
The persistence module handles file IO related towards persisting crew information.
`Data.Aeson` is able to derive serialization and deserialization for JSON automatically, so
the persistence module is very lightweight. Only providing additional functions to help with writing and reading 
crew files.

### Display
The display module handles rendering schedules as tables using Brick.
For the most part this is another lightweight module, providing helper functions that the main
module will make use of.

### Main
The main module found in `app/Main.hs` holds the main logic loop of the project.
The main logic loop keeps track of loaded crew members and generated schedules.
Users will be put into an interactive loop, being queried for what they would like to do, with any state
changes being tracked via the StateT monad.

In the event a user wants to create a crew file:
1. User uses `add crew manually` to load custom defined crew into memory
2. User then uses `save crew` to save the loaded crew into the given file path

In the event a user wants to view the best schedule generated from a given crew file:
1. User uses the `load crew` option to load crew from a given file.
2. User then uses the `generate schedules` option to generate schedules. Note: if this step is skipped the end user will only see an empty schedule when displaying the top schedule.
3. User then uses the `display schedule` option to show the schedule.

## Setup
This is a stack based project so you will need to have stack installed.
Stack should handle fetching the dependencies automatically.

For testing run:
```
  stack test
```

To run the code:
```
  stack run
```

## Additional Information

The `test.json` file is an example crew file you can use for testing the main logic loop.
