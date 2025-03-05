#include "scheduler.h"
#include <iostream>

using namespace std;


// Recursive backtracking function to schedule the matches
bool solve(Tournament tournament, Match matches[], int match_index, int NUM_VENUES_AND_DAYS, int NUM_TIMESLOTS) {
    // Base case: If all matches have been scheduled
    if (match_index == NUM_VENUES_AND_DAYS * NUM_TIMESLOTS) {
        return true; // All matches scheduled successfully
    }

    // For each possible time slot and venue, attempt to schedule the current match
    for (int day = 0; day < tournament.num_days; ++day) {
        
        for (int time = 0; time < NUM_TIMESLOTS; ++time) {
            
            // Calculate the index for the available time slot
            int index = (day * NUM_TIMESLOTS) + time; // Mapping (day, time) to 1D index

            // Try placing the match in the current time slot
            matches[match_index].venue = index % tournament.num_venues;  // Example calculation for venue
            
            matches[match_index].days = day; // Set the day
            
            matches[match_index].start = calculate_start_time(day, time, tournament.start_time, tournament.match_length); // Calculate start time
            
            matches[match_index].end = calculate_end_time(matches[match_index].start, tournament.match_length); // Calculate end time

            // Check if the move is valid (no conflicts with rest period or venue)
            if (is_valid(match_index, matches, tournament, NUM_VENUES_AND_DAYS, NUM_TIMESLOTS)) {
                matches[match_index].scheduler = true; // Mark the match as scheduled

                // Recursively try to schedule the next match
                if (solve(tournament, matches, match_index + 1, NUM_VENUES_AND_DAYS, NUM_TIMESLOTS)) {
                    return true; // If scheduling the next match succeeds, return true
                }

                // Backtrack: If the next match couldn't be scheduled, undo this move
                matches[match_index].scheduler = false; // Reset scheduler flag
            }
        }
    }

    // If no valid slot was found for this match, return false (backtrack)
    return false;


}


// Function to check if the current match can be scheduled
bool is_valid(int match_index, Match matches[], Tournament tournament, int NUM_VENUES_AND_DAYS, int NUM_TIMESLOTS) {
    
    Match current_match = matches[match_index];

    // Check if the match violates the rest period or venue-time conflicts
    for (int i = 0; i < match_index; ++i) {  // Check previous matches
        if ((matches[i].participant1 == current_match.participant1 || matches[i].participant2 == current_match.participant1) ||
            (matches[i].participant1 == current_match.participant2 || matches[i].participant2 == current_match.participant2)) {
            
           // Ensure the rest period is respected by comparing start and end times
        if (matches[i].end.hour > current_match.start.hour || 
            (matches[i].end.hour == current_match.start.hour && matches[i].end.minute > current_match.start.minute)) {
            return false; // Invalid if rest period violated
        }
     
        }
    }

    // Check for venue conflicts (i.e., no other match in the same venue/time slot)
    for (int i = 0; i < match_index; ++i) {
        if (matches[i].venue == current_match.venue && matches[i].days == current_match.days) {
            return false; // Venue conflict found
        }
    }

    return true;
}


// Main function to schedule the matches
void schedule_matches(Match matches[MAX_MATCHUPS], int TOTAL_MATCHUPS, Tournament tournament) 
{
    
    int NUM_VENUES_AND_DAYS = tournament.num_venues * tournament.num_days;
    int NUM_TIMESLOTS = get_interval(tournament.start_time, tournament.end_time) / tournament.match_length;

    // Initialize the scheduler status of all matches to false (unscheduled)
    for (int i = 0; i < TOTAL_MATCHUPS; ++i) {
        matches[i].scheduler = false;
    }

    // Call the backtracking function to schedule the matches
    if (solve(tournament, matches, 0, NUM_VENUES_AND_DAYS, NUM_TIMESLOTS)) {
        cout << "Schedule created successfully!" << endl;
    } else {
        cout << "Scheduling failed. Unable to generate a valid schedule based on the constraints." << endl;
    }
}


Time calculate_start_time(int day, int time_slot, Time start_time, int match_length) {
    // Calculate the total minutes since the start time
    int total_minutes = start_time.hour * 60 + start_time.minute + (time_slot * match_length);

    // Calculate the start time by converting total minutes back to hours and minutes
    Time start;
    start.hour = total_minutes / 60;   // Get the hours part
    start.minute = total_minutes % 60; // Get the minutes part

    return start;
}


Time calculate_end_time(Time start, int match_length) {
    // Calculate the total minutes for the match's end time
    int total_minutes = start.hour * 60 + start.minute + match_length;

    // Calculate the end time by converting total minutes back to hours and minutes
    Time end;
    end.hour = total_minutes / 60;   // Get the hours part
    end.minute = total_minutes % 60; // Get the minutes part

    return end;
}

