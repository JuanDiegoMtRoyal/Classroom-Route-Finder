import java.util.*;

public class Graph {
    private Map<String, Node> nodes; // Map of all nodes in the building
    private List<Hallway> hallways; // List of all hallways in the building

    public Graph() {
        nodes = new HashMap<>();
        hallways = new ArrayList<>();
    }

    public void addNode(Node node) {
        nodes.put(node.name, node);
    }

    public void addHallway(Hallway hallway) {
        hallways.add(hallway);
    }

    public List<Node> getNodesAlongHallway(Hallway hallway) {
        return hallway.getNodes();
    }

    public Node getNode(String name) {
        return nodes.get(name);
    }

    
}
