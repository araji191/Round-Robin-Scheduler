# Round Robin Tournament Scheduler

## Introduction

The Round Robin Tournament Scheduler is a software tool designed to generate a fair, conflict-free schedule for sports leagues using a round-robin format. It ensures that each team competes against every other team a set number of times in a single or double round-robin tournament. The tool efficiently manages logistical constraints such as venue availability, predefined time slots, and rules like mandatory rest periods to create a well-structured and balanced schedule.

By automating the scheduling process, this tool simplifies league planning, ensuring efficiency, fairness, and optimal venue utilization while dynamically handling complex constraints. It minimizes scheduling conflicts, ensures teams have adequate rest between matches, and organizes matches in a logical, structured manner. Designed as an essential resource for league organizers, the scheduler enhances tournament management by eliminating the challenges of manual scheduling and ensuring a seamless competition flow.

### Problem Instance

A soccer league wants to host a single round-robin tournament that features five teams in total. The tournament will span over two days, and the league has access to two venues throughout the span of the tournament. Matches can start as early as 9 AM, and matches can finish as late as 5 PM. To prevent the teams from playing consecutive matches and to allow for travel to different venues if necessary, there is a minimum 90-minute rest period between matches for every team.

## Inputs

To generate the tournament schedule, all the input will be read from a file that contains the following details:

- **Round-robin type** (“Single” or “Double”)
- **Participants**
- **Duration** (number of days)
- **Start Time** (24-hour format)
- **End Time** (24-hour format)
- **Match Length** (in minutes)
- **Number of Venues Available**
- **Rest Period** (in minutes)

The input file will first take the type of round-robin tournament, which can either be single or double, where the latter requires each team to play each other twice. Next, the file will take all the participants that will be a part of the tournament. Each participant will be separated by a new line in the file, and the program will stop reading when it reaches the `-1` terminator. The duration represents the number of days the tournament spans, allowing for matches to be held across many days. The daily start and end times are inputs that represent the time the tournament will start and end each day, using 24-hour time as the input. This is used to ensure that matches are scheduled within a specific time period, as well as compute the total number of hours that the tournament will run each day. The start and end times remain consistent throughout the duration of the tournament. The match length, recorded in minutes, is the maximum time that a match can take, being used to schedule games during the tournament. The **number of venues** indicates the number of available locations for matches to be held during the tournament each day, allowing for multiple matches to occur at the same time. Like the start and end times, the number of available venues remains consistent throughout the duration of the tournament. Finally, the rest period, also in minutes, specifies the minimum rest period for participants between matches, intended to avoid back-to-back games or for travel time between venues.

### Example Input File for Soccer Round-Robin Tournament (.txt or .dat file):

```
1     			-- round-robin type (1 indicates single round robin)
TEAM A                  -- list of participants
TEAM B
TEAM C
TEAM D
TEAM E
END                     -- finish reading list of teams
3		        -- number of days
9:00                    -- start time
17:00                   -- end time
90                      -- match length (in minutes)
2                       -- number of venues available
90                      -- rest period (in minutes)
```
“--” is used to represent comments in the input file.

## Valid Solution

Based on the participants, the program will get the number of teams represented by `numParticipants`, and use that value to compute the total number of matches that need to be scheduled during the tournament. To determine the number of matches that will occur, the following formula will be used:

Total number of matches = `(numParticipants² - numParticipants) / 2`

Each participant in the tournament will play `numParticipants - 1` matches in a single round-robin tournament. In the case of the soccer tournament example, since there are five teams, there will be a total of ten matches, with each team playing four matches. The following matches will need to be scheduled:

`TEAM A vs TEAM B`

`TEAM A vs TEAM C`

`TEAM A vs TEAM D`

`TEAM A vs TEAM E`

`TEAM B vs TEAM C`

`TEAM B vs TEAM D`

`TEAM B vs TEAM E`

`TEAM C vs TEAM D`

`TEAM C vs TEAM E`

`TEAM D vs TEAM E`


The program must get all the matches that need to be scheduled, then it will take the first match (`TEAM A vs TEAM B`) and place it in the first available spot (`DAY 1, Venue 1, 09:00-10:30`). It will then move on to the next match, where it searches for an available spot in `DAY 1, Venue 1`. Since it is another match played by `TEAM A`, there has to be at least a 90-minute gap after the first match. The program continues scheduling matches, filling up spots in different venues, but still making sure that the constraints, such as rest periods, are being taken into account. In this specific example, the program is able to schedule the first nine matches without much difficulty. However, when scheduling the final match (`TEAM D vs TEAM E`), it seems that there is no possible spot for the match to be scheduled while respecting the given constraints, regardless of the day or the venue. The program has to backtrack and reschedule the last successfully scheduled match (`TEAM C vs TEAM E`) and move to another available time slot. Doing this, the last match is able to be successfully scheduled, creating a valid schedule for the round-robin tournament.

### Example Output for Soccer Round-Robin Tournament (printed onto console):

```
DAY 1:
Venue 1:
  09:00-10:30: TEAM A vs TEAM B
  12:00-13:30: TEAM A vs TEAM C
  15:00-16:30: TEAM A vs TEAM D

Venue 2:
  09:00-10:30: TEAM C vs TEAM D
  12:00-13:30: TEAM B vs TEAM D
  15:00-16:30: TEAM B vs TEAM E

DAY 2:
Venue 1:
  09:00-10:30: TEAM A vs TEAM E
  10:30-12:00: TEAM B vs TEAM C
  12:00-13:30: TEAM D vs TEAM E
  15:00-16:30: TEAM C vs TEAM E
```

In the output, notice that:

- No team plays another game until after a minimum of 90 minutes have passed since their previous match has ended, regardless of venue.
- No team has multiple matches on the same day at the same time at different venues.
- Teams can have multiple matches at the same time, but on different days.
- No matches are scheduled on `Day 2 at Venue 2`, hence not printed onto the console.

If there is no possible schedule for the given input, then the console will print:

```
A schedule was not able to be generated based on the input
```

## Implementation

The programming language to be implemented in is C++ and VS Code as the development environment. A depth-first search approach will be used to explore and assign match schedules efficiently while adhering to constraints. The scheduling process will prioritize filling venues, days, and time slots in a structured order. Matches will first be assigned to the first venue before considering subsequent venues, ensuring optimal space utilization. Similarly, the scheduler will prioritize earlier days before moving to later ones, maintaining a logical match progression. Within each day, earlier time slots will be filled first, minimizing scheduling gaps and reducing the need for adjustments based on rest periods.

The program will represent match schedules using either a 1D or 2D array of structs, allowing efficient storage and manipulation of scheduling data. The scheduling system will be rigorously tested by varying key inputs such as the number of days, venues, teams, match durations, and required rest periods, ensuring robustness under different conditions. To further enhance flexibility, the program will support unconventional times (e.g., 9:57 or 11:13) to test adaptability under non-standard scheduling scenarios.

As an additional feature, the tool will handle double round-robin tournaments where each team plays against every other team twice, following the formula `(numTeams² - numTeams)` matches for `(numTeams)`. Through this structured approach, the scheduler will generate an organized, conflict-free match plan while accommodating complex constraints dynamically.
