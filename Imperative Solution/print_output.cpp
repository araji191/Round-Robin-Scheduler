#include "print_output.h"

bool compare_matches(const Match &a, const Match &b) {

    if (a.day != b.day)
        return a.day < b.day;

    if (a.venue != b.venue)
        return a.venue < b.venue;

    if (a.start.hour != b.start.hour)
        return a.start.hour < b.start.hour;

    return a.start.minute < b.start.minute;
}

void print_schedule(Match matches[], int total_matchups) {

    sort(matches, matches + total_matchups, compare_matches);

    int current_day = -1;
    int current_venue = -1;

    for (int i = 0; i < total_matchups; i++) {
        if (matches[i].day != current_day) {
            current_day = matches[i].day;
            cout << "\nDAY " << current_day << ":\n";
            current_venue = -1;
        }

        if (matches[i].venue != current_venue) {
            current_venue = matches[i].venue;
            cout << "Venue " << current_venue << ":\n";
        }

        cout << " ";
        if (matches[i].start.hour < 10)
            cout << "0";
        cout << matches[i].start.hour << ":";

        if (matches[i].start.minute < 10)
            cout << "0";
        cout << matches[i].start.minute << "-";

        if (matches[i].end.hour < 10)
            cout << "0";
        cout << matches[i].end.hour << ":";

        if (matches[i].end.minute < 10)
            cout << "0";
        cout << matches[i].end.minute << ": ";
        cout << matches[i].participant1 << " vs " << matches[i].participant2 << endl;
    }
}