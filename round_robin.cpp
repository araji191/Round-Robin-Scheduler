/*
 * round_robin.cpp
 * 
 * Main entry point for the round-robin tournament scheduler.
 * 
 * This program reads the tournament configuration from an input file, schedules all the matches
 * based on the specified parameters, and then prints the schedule. The scheduling uses a backtracking 
 * algorithm to assign time slots and venues, which is not very efficient for large tournaments.
 * 
 * The program performs the following tasks:
 * 1. Reads tournament data from an input file.
 * 2. Initializes the tournament and match setup.
 * 3. Calculates the total number of matchups based on the participants and tournament type.
 * 4. Schedules matches using the `schedule_matches` function.
 * 5. Prints the generated match schedule using the `print_schedule` function.
 * 
 *       **The backtracking scheduling algorithm used here may not be efficient for larger tournaments.
 *       This solution is suitable for small to medium-sized tournaments but may require optimization
 *       for handling a larger number of participants and matchups.**
 * 
 * Functions:
 * - main: The entry point for the program, responsible for reading input, setting up the tournament,
 *            and invoking the scheduling functions.
 * 
 * Error handling:
 * - If the input file cannot be opened or if no participants are found, the program will terminate with an error message.
 * - If the number of matchups exceeds the limit defined by `MAX_MATCHUPS`, the program will terminate.
 */

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

    // Open the input file containing tournament configuration    
    input_file.open(get_input_file());
    
    if (!input_file.is_open()) {
        cout << "Error: Could not open input file." << endl;
        return 1;
    }

    // Read the input data into the tournament structure    
    Tournament tournament;
    read_input(input_file, tournament);
    input_file.close();
    
    int num_participants = 0;
    // Count the number of participants    
    while (num_participants < MAX_PARTICIPANTS && !tournament.participants[num_participants].empty()) {
        num_participants++;
    }
    
    if (num_participants == 0) {
        cout << "Error: No participants found." << endl;
        return 1;
    }

    // Calculate the total number of matchups based on the number of participants and tournament type    
    int total_matchups = (pow(num_participants, 2) - num_participants) / 2 * tournament.type;
    if (total_matchups > MAX_MATCHUPS) {
        cout << "Error: Too many matchups for the current configuration." << endl;
        return 1;
    }

    // Initialize match array to store the scheduled matches    
    Match matches[MAX_MATCHUPS];
    
    int match_index = 0;
    // Generate matchups for the tournament, considering the tournament type (e.g., round-robin)    
    for (int k = 0; k < tournament.type; k++) {
        for (int i = 0; i < num_participants; i++) {
            for (int j = i + 1; j < num_participants; j++) {
                matches[match_index].participant1 = tournament.participants[i];
                matches[match_index].participant2 = tournament.participants[j];
                match_index++;
            }
        }
    }
    // Attempt to schedule the generated matchups
    bool success = schedule_matches(matches, total_matchups, tournament);
    // If scheduling is successful, print the generated schedule    
    if (success) {
        print_schedule(matches, total_matchups);
    }
    
    return 0;
}
