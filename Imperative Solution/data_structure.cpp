#include "data_structure.h"

int get_interval(Time start, Time end) {
    int start_minutes = start.hour * 60 + start.minute;
    int end_minutes = end.hour * 60 + end.minute;
    return end_minutes - start_minutes;
}
