// File name: round-robin.cpp
// Purpose: Main entry point for the round-robin tournament scheduler

#include <fstream>
#include <cmath>
#include <string>
#include "constants.h"
#include "data_structure.h"
#include "scheduler.h"
#include "read_input.h"
#include "print_output.h"

int main() {

    ifstream input_file;

    input_file.open(get_input_file());
    
    if (!input_file.is_open()) {
        cout << "Error: Could not open input file." << endl;
        return 1;
    }

    Tournament tournament;
    read_input(input_file, tournament);
    input_file.close();
    
    int num_participants = 0;
    while (num_participants < MAX_PARTICIPANTS && !tournament.participants[num_participants].empty()) {
        num_participants++;
    }
    
    if (num_participants == 0) {
        cout << "Error: No participants found." << endl;
        return 1;
    }
    
    int total_matchups = (pow(num_participants, 2) - num_participants) / 2 * tournament.type;
    if (total_matchups > MAX_MATCHUPS) {
        cout << "Error: Too many matchups for the current configuration." << endl;
        return 1;
    }
    
    Match matches[MAX_MATCHUPS];
    
    int match_index = 0;
    for (int k = 0; k < tournament.type; k++) {
        for (int i = 0; i < num_participants; i++) {
            for (int j = i + 1; j < num_participants; j++) {
                matches[match_index].participant1 = tournament.participants[i];
                matches[match_index].participant2 = tournament.participants[j];
                match_index++;
            }
        }
    }
    
    bool success = schedule_matches(matches, total_matchups, tournament);
    
    if (success) {
        print_schedule(matches, total_matchups);
    }
    
    return 0;
}