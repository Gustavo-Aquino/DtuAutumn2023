# -*- coding: utf-8 -*-
"""
Created on Wed Nov  1 09:04:49 2023

@author: Gustavo
"""

import pm4py
from pm4py.visualization.petri_net import visualizer as pn_visualizer
import pandas as pd
import os

# path to output datasets (excluding the general csv files)
data_directory = 'path_to/logs/file.csv'
fields_to_keep = ['case:concept:name', 'concept:name', 'time:timestamp']


def test_miner_alpha(data_directory, fields_to_keep):

    # List to store CSV files
    process_models = []
    test_logs = []
    
    # Table to store the values from the combinations between trained models and test logs
    fitness_table = pd.DataFrame(columns=['test_connector', 'train_connector', 'fitness'])
           
    # Iterate through connector types
    for connector_type in os.listdir(data_directory):
        connector_path = os.path.join(data_directory, connector_type)
    
        # Iterate through split types
        for split_type in os.listdir(connector_path):
            split_path = os.path.join(connector_path, split_type)
            
            if split_type == 'train':
                
                # Iterate through files in the folder
                for file_name in os.listdir(split_path):
                    file_path = os.path.join(split_path, file_name)
        
                    # Check if the path is a file and it has a xes extension
                    if os.path.isfile(file_path) and file_name.endswith('.xes'):
                        log_train = pm4py.read_xes(file_path)[fields_to_keep]
                        net, im, fm = pm4py.discover_petri_net_alpha(
                            log_train, 
                            activity_key='concept:name', 
                            case_id_key='case:concept:name', 
                            timestamp_key='time:timestamp'
                        )
                        model_info = {
                            'net': net,
                            'im': im,
                            'fm': fm,
                            'connector': connector_type
                        }
                        process_models.append(model_info)
                
            elif split_type == 'test':
                
                # Iterate through files in the folder
                for file_name in os.listdir(split_path):
                    file_path = os.path.join(split_path, file_name)
        
                    # Check if the path is a file and it has a xes extension
                    if os.path.isfile(file_path) and file_name.endswith('.xes'):
                        log_test= pm4py.read_xes(file_path)[fields_to_keep]
                        test_info = {
                            'log': log_test,
                            'label': connector_type,
                            'prediction_fit': '',
                            'fitness': 0,
                            'model_used': None
                        }
                        test_logs.append(test_info)
                
    # Iterate through the list of petri_nets and their information
    for test in test_logs:
        all_fitness = []
        for model in process_models:
            
            replayed_traces = pm4py.conformance_diagnostics_token_based_replay(test['log'], model['net'], model['im'], model['fm'])
            trace_fitness_values = [trace['trace_fitness'] for trace in replayed_traces]
            all_fitness.append(
                {'fitness':sum(trace_fitness_values) / len(trace_fitness_values),
                 'model_used':model,
                 'connector':model['connector']}
            )
            
            fitness_info = {
                'test_connector': test['label'],
                'train_connector': model['connector'],
                'fitness': [sum(trace_fitness_values) / len(trace_fitness_values)]}
            
            fitness_info = pd.DataFrame(fitness_info)
            
            # add all iterations to the table
            fitness_table = pd.concat([fitness_table, fitness_info], ignore_index = True)
        
        max_fitness = max(all_fitness, key=lambda x: x['fitness'])
        test['prediction_fit'] = max_fitness['connector']
        test['fitness'] = max_fitness['fitness']
        test['model_used'] = max_fitness['model_used']


    # Create a matrix showing the relationship between test_connector and train_connector
    results_matrix_fit = fitness_table.pivot(index='test_connector', columns='train_connector', values='fitness').T

    return([test_logs, results_matrix_fit])

def test_miner_heuri(data_directory, fields_to_keep, dependency_threshold):

    # List to store CSV files
    process_models = []
    test_logs = []
    
    fitness_table = pd.DataFrame(columns=['test_connector', 'train_connector', 'fitness'])
        
    # Iterate through connector types
    for connector_type in os.listdir(data_directory):
        connector_path = os.path.join(data_directory, connector_type)
    
        # Iterate through split types
        for split_type in os.listdir(connector_path):
            split_path = os.path.join(connector_path, split_type)
            
            if split_type == 'train':
                
                # Iterate through files in the folder
                for file_name in os.listdir(split_path):
                    file_path = os.path.join(split_path, file_name)
        
                    # Check if the path is a file and it has a xes extension
                    if os.path.isfile(file_path) and file_name.endswith('.xes'):
                        log_train = pm4py.read_xes(file_path)[fields_to_keep]
                        net, im, fm = pm4py.discover_petri_net_heuristics(
                            log_train,
                            dependency_threshold=dependency_threshold,
                            activity_key='concept:name', 
                            case_id_key='case:concept:name', 
                            timestamp_key='time:timestamp'
                        )
                                 
                        # For the heuristics miner we have the possibility to check the heuristics net
                        heu_net = pm4py.discover_heuristics_net(log_train)
                        model_info = {
                            'heu_net': heu_net,
                            'net': net,
                            'im': im,
                            'fm': fm,
                            'connector': connector_type}
                        
                        process_models.append(model_info)
                        
                
            elif split_type == 'test':
                
                # Iterate through files in the folder
                for file_name in os.listdir(split_path):
                    file_path = os.path.join(split_path, file_name)
        
                    # Check if the path is a file and it has a xes extension
                    if os.path.isfile(file_path) and file_name.endswith('.xes'):
                        log_test= pm4py.read_xes(file_path)[fields_to_keep]
                        test_info = {
                            'log': log_test,
                            'label': connector_type,
                            'prediction_fit': '',
                            'fitness': 0,
                            'model_used': None
                        }
                        test_logs.append(test_info)
                
    # Iterate through the list of petri_nets and their information
    for test in test_logs:
        all_fitness = []
        for model in process_models:
            
            replayed_traces = pm4py.conformance_diagnostics_token_based_replay(test['log'], model['net'], model['im'], model['fm'])
            trace_fitness_values = [trace['trace_fitness'] for trace in replayed_traces]
            all_fitness.append(
                {'fitness':sum(trace_fitness_values) / len(trace_fitness_values),
                 'model_used':model,
                 'connector':model['connector']}
            )
            
            fitness_info = {
                'test_connector': test['label'],
                'train_connector': model['connector'],
                'fitness': [sum(trace_fitness_values) / len(trace_fitness_values)]}
            
            fitness_info = pd.DataFrame(fitness_info)
            
            # add all iterations to the table
            fitness_table = pd.concat([fitness_table, fitness_info], ignore_index = True)
        
        max_fitness = max(all_fitness, key=lambda x: x['fitness'])
        test['prediction_fit'] = max_fitness['connector']
        test['fitness'] = max_fitness['fitness']
        test['model_used'] = max_fitness['model_used']


    # Create a matrix showing the relationship between test_connector and train_connector
    results_matrix_fit = fitness_table.pivot(index='test_connector', columns='train_connector', values='fitness').T

    return([test_logs, results_matrix_fit])

def test_miner_induc(data_directory, fields_to_keep, noise_threshold):

    # List to store CSV files
    process_models = []
    test_logs = []
    
    fitness_table = pd.DataFrame(columns=['test_connector', 'train_connector', 'fitness'])
        
    # Iterate through connector types
    for connector_type in os.listdir(data_directory):
        connector_path = os.path.join(data_directory, connector_type)
    
        # Iterate through split types
        for split_type in os.listdir(connector_path):
            split_path = os.path.join(connector_path, split_type)
            
            if split_type == 'train':
                
                # Iterate through files in the folder
                for file_name in os.listdir(split_path):
                    file_path = os.path.join(split_path, file_name)
        
                    # Check if the path is a file and it has a xes extension
                    if os.path.isfile(file_path) and file_name.endswith('.xes'):
                        log_train = pm4py.read_xes(file_path)[fields_to_keep]
                        net, im, fm = pm4py.discover_petri_net_inductive(
                            log_train,
                            noise_threshold=noise_threshold,
                            activity_key='concept:name', 
                            case_id_key='case:concept:name', 
                            timestamp_key='time:timestamp'
                        )
                        
                        # For the inductive miner we have the possibility to check the process tree
                        process_tree = pm4py.discover_process_tree_inductive(
                            log_train,
                            noise_threshold=noise_threshold,
                            activity_key='concept:name', 
                            case_id_key='case:concept:name', 
                            timestamp_key='time:timestamp'
                        )
                        model_info = {
                            'p_tree': process_tree,
                            'net': net,
                            'im': im,
                            'fm': fm,
                            'connector': connector_type
                        }
                        process_models.append(model_info)
                                        
            elif split_type == 'test':
                
                # Iterate through files in the folder
                for file_name in os.listdir(split_path):
                    file_path = os.path.join(split_path, file_name)
        
                    # Check if the path is a file and it has a xes extension
                    if os.path.isfile(file_path) and file_name.endswith('.xes'):
                        log_test= pm4py.read_xes(file_path)[fields_to_keep]
                        test_info = {
                            'log': log_test,
                            'label': connector_type,
                            'prediction_fit': '',
                            'fitness': 0,
                            'model_used': None
                        }
                        test_logs.append(test_info)
                
    # Iterate through the list of petri_nets and their information
    for test in test_logs:
        all_fitness = []
        for model in process_models:
            
            replayed_traces = pm4py.conformance_diagnostics_token_based_replay(test['log'], model['net'], model['im'], model['fm'])
            trace_fitness_values = [trace['trace_fitness'] for trace in replayed_traces]
            all_fitness.append(
                {'fitness':sum(trace_fitness_values) / len(trace_fitness_values),
                 'model_used':model,
                 'connector':model['connector']}
            )
            
            fitness_info = {
                'test_connector': test['label'],
                'train_connector': model['connector'],
                'fitness': [sum(trace_fitness_values) / len(trace_fitness_values)]}
            
            fitness_info = pd.DataFrame(fitness_info)
            
            # add all iterations to the table
            fitness_table = pd.concat([fitness_table, fitness_info], ignore_index = True)
        
        max_fitness = max(all_fitness, key=lambda x: x['fitness'])
        test['prediction_fit'] = max_fitness['connector']
        test['fitness'] = max_fitness['fitness']
        test['model_used'] = max_fitness['model_used']


    # Create a matrix showing the relationship between test_connector and train_connector
    results_matrix_fit = fitness_table.pivot(index='test_connector', columns='train_connector', values='fitness').T

    return([test_logs, results_matrix_fit])

def view_PetriNet(model):
    pm4py.view_petri_net(
        petri_net = model['net'],
        initial_marking = model['im'],
        final_marking = model['fm'],
        format='svg'
        )
    pass

def save_net(model, direction = 'LR'):
    pm4py.save_vis_petri_net(
        petri_net = model['net'],
        initial_marking = model['im'],
        final_marking = model['fm'],
        file_path = 'petri_net.png',
        rankdir= direction
        )
    pass

def view_heu_net(model):
    pm4py.view_heuristics_net(
        model['heu_net'],
        format='svg'
        )
    pass

def view_process_tree(model):
    pm4py.view_process_tree(
        model['p_tree'], 
        format='svg'
        )
    pass

# Results for each miner
# results_alpha = test_miner_alpha(data_directory, fields_to_keep)
# results_heuri = test_miner_heuri(data_directory, fields_to_keep, dependency_threshold = 0.7)
# results_induc = test_miner_induc(data_directory, fields_to_keep, noise_threshold = 0.6)

# How to visualize the nets    
# view_PetriNet(results_alpha[0][0]['model_used'])
# save_net(results_alpha[0][0]['model_used'])
# view_heu_net(results_heuri[0][0]['model_used'])
# view_process_tree(results_induc[0][0]['model_used'])



