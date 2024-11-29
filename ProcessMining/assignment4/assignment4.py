# -*- coding: utf-8 -*-
"""
Created on Tue Oct 10 15:32:52 2023

@author: Gustavo
"""

from datetime import datetime
import xml.etree.ElementTree as ET
import itertools
import copy

class PetriNet():

    def __init__(self):
        self.places = {}  # Dictionary to store places and their tokens
        self.transitions = {}  # Dictionary to store transitions and their status (enabled/disabled)
        self.edges = []  # List to store edges (connections between places and transitions)

    def add_place(self, id):
        if id in self.places.keys(): 
            print('Trying to add existing place')
            return self
        
        self.places[id] = 0  # Initialize the place with 0 tokens
        return self  # Return self for method chaining

    def add_transition(self, id, name):
        if id in self.transitions.keys(): 
            print('Trying to add existing transition')
            return self
        
        self.transitions[id] = name  # Initialize the transition as disabled, not using right now
        return self  # Return self for method chaining

    def add_edge(self, source, target):
        self.edges.append((source, target))  # Add the edge to the list
        return self  # Return self for method chaining
    
    def add_marking(self, place):
        self.places[place] += 1  # Add one token to the specified place

    def get_tokens(self, place):
        return self.places.get(place, 0)  # Return the number of tokens in the specified place, defaulting to 0 if not found

    def places_required(self, transition):
        pr = []
        for source, target in self.edges:
            if target == transition:
                pr.append(source)
        return pr

    def tokens_produced(self, transition):
        tg = 0
        for source, target in self.edges:
            if source == transition:
                tg += 1
        return tg
    
    def tokens_consumed(self, transition):
        tc = 0
        for source, target in self.edges:
            if target == transition:
                tc += 1
        return tc

    def is_enabled(self, transition):
        for source, target in self.edges:
            if target == transition and self.places[source] < 1:
                return False
        return True

    def fire_transition(self, transition):
        if self.is_enabled(transition):
            for source, target in self.edges:
                if target == transition:
                    self.places[source] -= 1
                if source == transition:
                    self.places[target] += 1

    def transition_name_to_id(self, name):
        for key, value in self.transitions.items():
            if value == name: return key
            
        return 'ERRO'
    
    def reset(self):
        for place in self.places:
            self.places[place] = 0

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
            if event.find('{http://www.xes-standard.org/}int'):
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

def create_footprint_matrix(event_log):
    footprint_matrix = {}

    # Helper function to check if x is directly followed by y
    def is_direct_succession(x, y):
        for trace in event_log:
            if x in trace:
                x_index = trace.index(x)
                if x_index < len(trace) - 1 and trace[x_index + 1] == y:
                    return True
        return False

    # Helper function to check if x causes y
    def is_causality(x, y):
        return is_direct_succession(x, y) and not is_direct_succession(y, x)

    # Helper function to check if x and y are parallel
    def is_parallel(x, y):
        return is_direct_succession(x, y) and is_direct_succession(y, x)

    # Helper function to check if x and y are in a choice relation
    def is_choice(x, y):
        return not is_direct_succession(x, y) and not is_direct_succession(y, x)

    # Initialize the footprint matrix
    activities = set(activity for trace in event_log for activity in trace)
    for x in activities:
        footprint_matrix[x] = {}
        for y in activities:
            if x != y:
                if is_causality(x, y):
                    footprint_matrix[x][y] = 'Right Causality'
                elif is_causality(y, x):
                    footprint_matrix[x][y] = 'Left Causality'
                elif is_parallel(x, y):
                    footprint_matrix[x][y] = 'Parallel'
                elif is_choice(x, y):
                    footprint_matrix[x][y] = 'Choice'
                elif is_direct_succession(x, y):
                    footprint_matrix[x][y] = 'Direct Succession'
            elif x == y:
                footprint_matrix[x][y] = 'Choice'
    return footprint_matrix

def check_footprint_matrix(subset_1, subset_2, relation, footprint):
    for event1 in subset_1:
        for event2 in subset_2:
            if footprint[event1].get(event2) != relation: 
                return False
    
    return True

def find_activity_patterns(footprint_matrix):
    patterns = []

    for x in footprint_matrix:
        for y in footprint_matrix:
            if x != y:
                if footprint_matrix[x].get(y) == 'Right Causality':
                    # Sequence Pattern (x -> y)
                    sequence_pattern = [x, y]
                    patterns.append(sequence_pattern)

                    # XOR Split Pattern
                    xor_split_pattern = []
                    for z in footprint_matrix:
                        if z != x and z != y:
                            if (
                                footprint_matrix[x].get(z) == 'Right Causality' and
                                footprint_matrix[y].get(z) == 'Choice'
                            ):
                                xor_split_pattern = [x, [y, z]]
                    if xor_split_pattern:
                        patterns.append(xor_split_pattern)

                    # AND Split Pattern
                    and_split_pattern = []
                    for z in footprint_matrix:
                        if z != x and z != y:
                            if (
                                footprint_matrix[x].get(z) == 'Right Causality' and
                                footprint_matrix[y].get(z) == 'Parallel'
                            ):
                                and_split_pattern = [x, [y, z]]
                    if and_split_pattern:
                        patterns.append(and_split_pattern)

                if footprint_matrix[x].get(y) == 'Choice':
                    # XOR Join Pattern
                    xor_join_pattern = []
                    for z in footprint_matrix:
                        if z != x and z != y:
                            if (
                                footprint_matrix[x].get(z) == 'Right Causality' and
                                footprint_matrix[y].get(z) == 'Right Causality'
                            ):
                                xor_join_pattern = [[x, y], z]
                    if xor_join_pattern:
                        patterns.append(xor_join_pattern)


                if footprint_matrix[x].get(y) == 'Parallel':
                    # AND Join Pattern
                    and_join_pattern = []
                    for z in footprint_matrix:
                        if z != x and z != y:
                            if (
                                footprint_matrix[x].get(z) == 'Right Causality' and
                                footprint_matrix[y].get(z) == 'Right Causality'
                            ):
                                and_join_pattern = [[x, y], z]
                    if and_join_pattern:
                        patterns.append(and_join_pattern)
            
    return patterns

def alpha(event_log):
    # Initialize the Alpha Miner
    cases = list(event_log.values())

    workflow_log = []
    T_i = set()
    T_o = set()

    for case in cases:
       workflow = [entry['concept:name'] for entry in case]
       T_i.add(workflow[0])
       T_o.add(workflow[-1])
       workflow_log.append(workflow)
       
    # get only unique traces of activities
    workflow_log = [list(x) for x in set(tuple(x) for x in workflow_log)]
   
    # unique activities
    T_l = list(set(x for l in workflow_log for x in l))
      
    footprint_matrix = create_footprint_matrix(workflow_log)
        
    # print(footprint_matrix)
    
    # all pairs
    xl = set()
    subsets = set()
    for i in range(1, len(T_l)):
        for s in itertools.combinations(T_l, i):
            subsets.add(s)
    # after subsets created check conditions to add in pairs
    for a in subsets:
        check_a = check_footprint_matrix(a,a,'Choice',footprint_matrix)
        for b in subsets:
            check_b = check_footprint_matrix(b,b,'Choice',footprint_matrix)
            if check_a and check_b and check_footprint_matrix(a,b,'Right Causality', footprint_matrix):
                xl.add((a,b))
    # print(xl)
    # maximal pairs
    yl = copy.deepcopy(xl)
    for a in xl:
        a_A = a[0]
        a_B = a[1]
        for b in xl:
            if a != b:
                if set(a_A).issubset(b[0]) and set(a_B).issubset(b[1]):
                    yl.discard(a)
    
    yl = list(yl)
    # print(yl)
    
    # Initialize a Petri Net
    petri_net = PetriNet()
        
    # all the transitions    
    for transition in T_l:
        petri_net.add_transition( (-T_l.index(transition)-1), transition)
    
    # all places: input, output, one place for each pair
    petri_net.add_place(100)
    petri_net.add_place(101)
    for place in yl:
        petri_net.add_place(yl.index(place)+1)
        
    # go through the pairs and make the connections between each element of them
    for (source, target) in yl:
        for src in source:
            petri_net.add_edge( (-T_l.index(src)-1), yl.index((source, target))+1)
        for trgt in target:
            petri_net.add_edge( yl.index((source,target))+1, (-T_l.index(trgt)-1))
    
    # connect artificial input place to the initial transitions            
    for ini in T_i:
        petri_net.add_edge(100, (-T_l.index(ini)-1))
    # connect artificial output place to the final transitions            
    for end in T_o: 
        petri_net.add_edge((-T_l.index(end)-1), 101)

    return petri_net

def check_enabled(pn):
  # ts = ["record issue", "inspection", "intervention authorization", "action not required", "work mandate", "no concession", "work completion", "issue completion"]
  ts = ['register application', 'check credit', 'calculate capacity', 'check system', 'accept', 'send decision e-mail']
  for t in ts:
    print (t,pn.is_enabled(pn.transition_name_to_id(t)))
  print("")

def fitness_token_replay(log, model):
    cases = list(log.values())

    workflow_log = []

    for case in cases:
       workflow = [entry['concept:name'] for entry in case]
       workflow_log.append(workflow)
    
    visited = []
    
    # Initialize variables to keep track of the sums for both components
    missing = 0
    consumed = 0
    remaining = 0
    produced = 0

    # Iterate through each trace in the event log
    for trace in workflow_log:
        if trace in visited: continue        
        # Initialize variables for this trace
        ni = workflow_log.count(trace)
        mi = 0
        ci = 0
        ri = 0
        pi = 0

        model.add_marking(100)
        pi += 1        

        for transition in trace:
            t_id = model.transition_name_to_id(transition)
            if not model.is_enabled(t_id):
                for place in model.places_required(t_id): 
                    model.add_marking(place)
                    mi += 1
            
            model.fire_transition(t_id)
            ci += model.tokens_consumed(t_id)
            pi += model.tokens_produced(t_id)

        if model.get_tokens(101) == 0:
            mi += 1
            ci += 1
        else:
            ci += 1
            model.places[101] -= 1
            
        ri += sum(model.places.values())
        
        model.reset()


        # CHECK whether we consume all tokens in end or only one
        # Update the sums for both components
        missing += ni * mi
        consumed += ni * ci
        remaining += ni * ri
        produced += ni * pi
        
        visited.append(trace)

    # Calculate the first fitness component
    component1 = 1 - (missing / consumed)

    # Calculate the second fitness component
    component2 = 1 - (remaining / produced)

    # Calculate the overall 'f' metric
    f = 0.5 * component1 + 0.5 * component2

    return f

# if __name__ == "__main__":
    
#     log = read_from_file('extension-log-4.xes')
#     noisy_log =read_from_file('extension-log-noisy-4.xes')
    
#     mined_model = alpha(log)

#     correto = fitness_token_replay(log, mined_model)
#     teste = fitness_token_replay(noisy_log, mined_model)
    