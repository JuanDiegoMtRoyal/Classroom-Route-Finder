import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class Intersection extends Node {
    private List<Hallway> connectedHallways; // Tracks all connected hallways
    private List<Node> connectedNodes; // Other connections (stairs, elevators)

    public Intersection(String name, int positionAlongHallway, int floor) {
        super(name, null, null, positionAlongHallway, floor);
        this.connectedHallways = new ArrayList<>();
        this.connectedNodes = new ArrayList<>();

    }

    public void addHallway(Hallway hallway) {
        if (!connectedHallways.contains(hallway)) {
            connectedHallways.add(hallway);
        }
    }

    public List<Hallway> getConnectedHallways() {
        return connectedHallways;
    }

    // Other node connections (stairs, elevators, prev/next classroom)
    public void addConnectedNode(Node node) {
        if (!connectedNodes.contains(node) && !(node instanceof Intersection)) {
            connectedNodes.add(node);
        }
    }

    public List<Node> getConnectedNodes() {
        return connectedNodes.stream()
                .sorted(Comparator.comparingInt(n -> n.positionAlongHallway))
                .collect(Collectors.toList());
    }

    @Override
    public void displayInfo() {
        System.out.println("\n-Arrive at Intersection: " + name + "\n*From here: ");
    }
}
