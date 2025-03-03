import java.util.*;

public class Graph {
    private Map<String, Node> nodes; // Map of all nodes in the building
    private List<Hallway> hallways; // List of all hallways in the building

    public Graph() {
        nodes = new HashMap<>();
        hallways = new ArrayList<>();
    }


    /**
     * A getter function
     * @return
     */
    public Collection<Node> getAllNodes() {
        return nodes.values();
    }

/**
 * @brief 
 *
 * 
 *
 * @param 
 */
    public void addNode(Node node) {
        nodes.put(node.name, node);
    }

/**
 * @brief 
 *
 * 
 *
 * @param 
 */
    public void addHallway(Hallway hallway) {
        hallways.add(hallway);
    }

/**
 * @brief 
 *
 * 
 *
 * @param 
 */
    public List<Node> getNodesAlongHallway(Hallway hallway) {
        return hallway.getNodes();
    }

        
/**
 * @brief 
 *
 * 
 *
 * @param 
 */
    public Node getNode(String name) {
        return nodes.get(name);
    }

/**
 * @brief 
 *
 * 
 *
 * @param 
 */
    public List<Hallway> getHallways() {
        return hallways;
    }
    
}
