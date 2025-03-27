import java.util.*;

public class Graph {
    private Map<String, Node> nodes; // Map of all nodes in the building
    private List<Hallway> hallways; // List of all hallways in the building
    private List<Intersection> intersections; // List of all hallways in the building

    public Graph() {
        nodes = new HashMap<>();
        hallways = new ArrayList<>();
        intersections = new ArrayList<>();
    }

    /**
     * A getter function
     * 
     * @return
     */
    public Collection<Node> getAllNodes() {
        return nodes.values();
    }

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

    public void addIntersection(Intersection intersection) {
        intersections.add(intersection);
    }

    public List<Node> getNodesAlongHallway(Hallway hallway) {
        return hallway.getNodes();
    }

    public Node getNode(String name) {
        return nodes.get(name);
    }

    public List<Intersection> getIntersections() {
        return intersections;
    }

    public List<Hallway> getHallways() {
        return hallways;
    }

  
}
