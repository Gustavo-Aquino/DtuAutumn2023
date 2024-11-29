class PetriNet():

    def __init__(self):
        self.places = {}  # Dictionary to store places and their tokens
        self.transitions = {}  # Dictionary to store transitions and their status (enabled/disabled)
        self.edges = []  # List to store edges (connections between places and transitions)

    def add_place(self, id):
        self.places[id] = 0  # Initialize the place with 0 tokens
        return self  # Return self for method chaining

    def add_transition(self, name, id):
        self.transitions[id] = False  # Initialize the transition as disabled
        return self  # Return self for method chaining

    def add_edge(self, source, target):
        self.edges.append((source, target))  # Add the edge to the list
        return self  # Return self for method chaining
    
    def add_marking(self, place):
        self.places[place] += 1  # Add one token to the specified place

    def get_tokens(self, place):
        return self.places.get(place, 0)  # Return the number of tokens in the specified place, defaulting to 0 if not found

    def is_enabled(self, transition):
        if self.transitions[transition] == False:
            for source, target in self.edges:
                if target == transition and self.places.get(source, 0) == 0:
                    return False
            return True
        return False

    def fire_transition(self, transition):
        if self.is_enabled(transition):
            for source, target in self.edges:
                if target == transition:
                    self.places[source] -= 1
                if source == transition:
                    self.places[target] += 1

# if __name__ == "__main__":
#     p = PetriNet()

#     p.add_place(1)  # add place with id 1
#     p.add_place(2)
#     p.add_place(3)
#     p.add_place(4)
#     p.add_transition("A", -1)  # add transition "A" with id -1
#     p.add_transition("B", -2)
#     p.add_transition("C", -3)
#     p.add_transition("D", -4)

#     p.add_edge(1, -1)
#     p.add_edge(-1, 2)
#     p.add_edge(2, -2).add_edge(-2, 3)
#     p.add_edge(2, -3).add_edge(-3, 3)
#     p.add_edge(3, -4)
#     p.add_edge(-4, 4)

#     # print(p.places)
#     # print(p.transitions)
#     # print(p.edges)
    
#     print(p.is_enabled(-1), p.is_enabled(-2), p.is_enabled(-3), p.is_enabled(-4))

#     p.add_marking(1)  # add one token to place id 1
#     print(p.is_enabled(-1), p.is_enabled(-2), p.is_enabled(-3), p.is_enabled(-4))

#     p.fire_transition(-1)  # fire transition A
#     print(p.is_enabled(-1), p.is_enabled(-2), p.is_enabled(-3), p.is_enabled(-4))

#     p.fire_transition(-3)  # fire transition C
#     print(p.is_enabled(-1), p.is_enabled(-2), p.is_enabled(-3), p.is_enabled(-4))

#     p.fire_transition(-4)  # fire transition D
#     print(p.is_enabled(-1), p.is_enabled(-2), p.is_enabled(-3), p.is_enabled(-4))

#     p.add_marking(2)  # add one token to place id 2
#     print(p.is_enabled(-1), p.is_enabled(-2), p.is_enabled(-3), p.is_enabled(-4))

#     p.fire_transition(-2)  # fire transition B
#     print(p.is_enabled(-1), p.is_enabled(-2), p.is_enabled(-3), p.is_enabled(-4))

#     p.fire_transition(-4)  # fire transition D
#     print(p.is_enabled(-1), p.is_enabled(-2), p.is_enabled(-3), p.is_enabled(-4))

#     # by the end of the execution, there should be 2 tokens on the final place
#     print(p.get_tokens(4))