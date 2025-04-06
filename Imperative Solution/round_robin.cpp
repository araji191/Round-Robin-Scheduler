/*
 * File name: round_robin.cpp
 * 
 * Authors: Abiola Raji, Ochihai Omuha
 * 
 * Main program file for the Round Robin Tournament Scheduler.
 * Handles program execution including input reading, tournament setup,
 * match scheduling, and output generation.
 * 
 * Functions:
 * - main: Program entry point that coordinates all scheduling operations
 */

#include <fstream>
#include <cmath>
#include <string>
#include "constants.h"
#include "data_structure.h"
#include "scheduler.h"
#include "read_input.h"
#include "print_output.h"

int main(int argc, char* argv[]) {
    if (argc != 2) {
        cout << "Please provide an input file as follows:" << endl;
        cout << endl;
        cout << "  " << argv[0] << " <input_file>" << endl << endl;
        cout << "Note: The program will automatically look for the file in the 'testcases' directory." << endl;
        return 1;
    }

    string input_file_path = "../testcases/" + string(argv[1]);
    ifstream input_file(input_file_path);
    
    if (!input_file.is_open()) {
        cout << "Error: Could not open input file: " << input_file_path << endl;
        return 1;
    }

    Tournament tournament;
    read_input(input_file, tournament);
    input_file.close();
    
    int num_participants = count_tournament_participants(tournament);
    
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
    
    generate_matchups(tournament, matches, num_participants, match_index);
    
    bool success = schedule_matches(matches, total_matchups, tournament);
    
    if (success) {
        print_schedule(matches, total_matchups);
    } else {
        cout << "Failed to generate schedule" << endl;
    }
    
    return 0;
}