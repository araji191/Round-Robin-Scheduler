#include "scheduler.h"
#include <algorithm>
#include <iostream>

using namespace std;

// Helper function to print debug info
void print_debug(const Match& m, const string& action) {
    cout << action << ": " << m.participant1 << " vs " << m.participant2 
         << " Day " << m.day << " Venue " << m.venue 
         << " " << m.start.hour << ":" << (m.start.minute < 10 ? "0" : "") << m.start.minute
         << "-" << m.end.hour << ":" << (m.end.minute < 10 ? "0" : "") << m.end.minute << endl;
}

bool compareMatches(const Match &a, const Match &b) {
    return (a.participant1 < b.participant1) || 
           (a.participant1 == b.participant1 && a.participant2 < b.participant2);
}

bool schedule_matches(Match matches[], int total_matchups, Tournament &tournament) {
    // Initialize all matches
    for (int i = 0; i < total_matchups; i++) {
        matches[i].scheduled = false;
        matches[i].day = 0;
        matches[i].venue = 0;
        matches[i].start.hour = 0;
        matches[i].start.minute = 0;
        matches[i].end.hour = 0;
        matches[i].end.minute = 0;
    }

    cout << "Starting scheduling for " << total_matchups << " matches..." << endl;
    
    // Sort matches by participants
    sort(matches, matches + total_matchups, compareMatches);
    
    bool success = solve(matches, 0, total_matchups, tournament);
    
    if (success) {
        cout << "Schedule created successfully!" << endl;
        return true;
    } else {
        cout << "Failed to create schedule" << endl;
        return false;
    }
}

bool solve(Match matches[], int match_index, int total_matchups, Tournament &tournament) {
    if (match_index >= total_matchups) {
        return true;
    }

    Match &current = matches[match_index];
    cout << "Trying to schedule match " << match_index + 1 << "/" << total_matchups 
         << ": " << current.participant1 << " vs " << current.participant2 << endl;

    int day_start_min = tournament.start_time.hour * 60 + tournament.start_time.minute;
    int day_end_min = tournament.end_time.hour * 60 + tournament.end_time.minute;
    int match_length = tournament.match_length;

    for (int day = 1; day <= tournament.num_days; day++) {
        for (int venue = 1; venue <= tournament.num_venues; venue++) {
            for (int start_min = day_start_min; start_min <= day_end_min - match_length; start_min += 30) {
                Time start = {start_min / 60, start_min % 60};
                Time end = {(start_min + match_length) / 60, (start_min + match_length) % 60};

                current.day = day;
                current.venue = venue;
                current.start = start;
                current.end = end;

                if (is_valid(matches, match_index, tournament)) {
                    current.scheduled = true;
                    print_debug(current, "Attempting");

                    if (solve(matches, match_index + 1, total_matchups, tournament)) {
                        return true;
                    }

                    current.scheduled = false;
                    print_debug(current, "Backtracking");
                }
            }
        }
    }
    return false;
}

bool is_valid(Match matches[], int match_index, Tournament &tournament) {
    const Match &current = matches[match_index];
    int current_start = current.start.hour * 60 + current.start.minute;
    int current_end = current.end.hour * 60 + current.end.minute;

    for (int i = 0; i < match_index; i++) {
        if (!matches[i].scheduled) continue;

        // Venue conflict check
        if (matches[i].day == current.day && matches[i].venue == current.venue) {
            int other_start = matches[i].start.hour * 60 + matches[i].start.minute;
            int other_end = matches[i].end.hour * 60 + matches[i].end.minute;

            if ((current_start >= other_start && current_start < other_end) ||
                (other_start >= current_start && other_start < current_end)) {
                return false;
            }
        }

        // Participant conflict check
        if ((current.participant1 == matches[i].participant1 || 
             current.participant1 == matches[i].participant2 ||
             current.participant2 == matches[i].participant1 || 
             current.participant2 == matches[i].participant2) &&
             current.day == matches[i].day) {

            int other_start = matches[i].start.hour * 60 + matches[i].start.minute;
            int other_end = matches[i].end.hour * 60 + matches[i].end.minute;

            // Rest period check
            if ((current_start < other_end + tournament.rest_period && current_end > other_start) ||
                (other_start < current_end + tournament.rest_period && other_end > current_start)) {
                return false;
            }
        }
    }
    return true;
}