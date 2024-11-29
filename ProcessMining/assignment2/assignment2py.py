from datetime import datetime
import xml.etree.ElementTree as ET

def element_to_dict(element):
    result = {}
    for child in element:
        if len(child) == 0:
            result[child.attrib['key']] = child.attrib['value']
        else:
            if child.tag not in result:
                result[child.tag] = []
            result[child.tag].append(element_to_dict(child))
    return result

def read_from_file(filename):
    clean_log = {}
    
    trace_list = []
    
    # Load the .xes file
    tree = ET.parse(filename)
    root = tree.getroot()
    
    # Iterate through the trace elements and convert them to dictionaries
    for trace in root.findall('.//{http://www.xes-standard.org/}trace'):
        # Convert time:timestamp to datetime
        for event in trace.iter('{http://www.xes-standard.org/}event'):
            timestamp_str = event.find('{http://www.xes-standard.org/}date').attrib['value']
            timestamp = datetime.strptime(timestamp_str, "%Y-%m-%dT%H:%M:%S%z").replace(tzinfo=None)
            event.find('{http://www.xes-standard.org/}date').attrib['value'] = timestamp
            
            cost_str = event.find('{http://www.xes-standard.org/}int').attrib['value']
            cost = int(cost_str)
            event.find('{http://www.xes-standard.org/}int').attrib['value'] = cost
        
        trace_dict = element_to_dict(trace)
        
        trace_list.append(trace_dict)
    
    for full_case in trace_list:
        case_id = full_case['concept:name']
        events_list = full_case['{http://www.xes-standard.org/}event']
        
        clean_log[case_id] = events_list
        
    return clean_log
        
def log_as_dictionary(log):
    log_dict = {}  # Initialize an empty dictionary to store the log

    # Split the input log by lines to process each event
    lines = log.strip().split('\n')

    for line in lines:
        event_info = line.strip().split(';')
        if len(event_info) == 4:
            activity, case_id, user, timestamp_str = event_info

            # Convert the timestamp string to a datetime object
            timestamp = datetime.strptime(timestamp_str, '%Y-%m-%d %H:%M:%S')

            # Create a dictionary to represent the event
            event = {
                'activity': activity,
                'case_id': case_id,
                'user': user,
                'timestamp': timestamp
            }

            # Check if the case_id already exists in the dictionary
            if case_id in log_dict:
                log_dict[case_id].append(event)
            else:
                log_dict[case_id] = [event]

    return log_dict

def dependency_graph_inline(log):
    dependency_graph = {}  # Initialize an empty dictionary to store the dependency graph

    # Iterate through cases in the log
    for case_id, events in log.items():
        # Iterate through events within each case
        for i in range(len(events) - 1):
            current_event = events[i]
            next_event = events[i + 1]

            # Extract the current and next activities
            current_activity = current_event['activity']
            next_activity = next_event['activity']

            # Check if the current activity is in the dependency graph
            if current_activity in dependency_graph:
                # Check if the next activity is already a key in the sub-dictionary
                if next_activity in dependency_graph[current_activity]:
                    # Increment the frequency of the relationship
                    dependency_graph[current_activity][next_activity] += 1
                else:
                    # Create a new entry for the next activity
                    dependency_graph[current_activity][next_activity] = 1
            else:
                # Create a new entry for the current activity and the next activity
                dependency_graph[current_activity] = {next_activity: 1}

    return dependency_graph

def dependency_graph_file(log):
    dependency_graph = {}  # Initialize an empty dictionary to store the dependency graph

    # Iterate through cases in the log
    for case_id, events in log.items():
        # Iterate through events within each case
        for i in range(len(events) - 1):
            current_event = events[i]
            next_event = events[i + 1]

            # Extract the current and next activities
            current_activity = current_event['concept:name']
            next_activity = next_event['concept:name']

            # Check if the current activity is in the dependency graph
            if current_activity in dependency_graph:
                # Check if the next activity is already a key in the sub-dictionary
                if next_activity in dependency_graph[current_activity]:
                    # Increment the frequency of the relationship
                    dependency_graph[current_activity][next_activity] += 1
                else:
                    # Create a new entry for the next activity
                    dependency_graph[current_activity][next_activity] = 1
            else:
                # Create a new entry for the current activity and the next activity
                dependency_graph[current_activity] = {next_activity: 1}

    return dependency_graph
