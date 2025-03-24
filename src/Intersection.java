import java.util.ArrayList;
import java.util.List;

public class Intersection extends Node {
    private List<Hallway> connectedHallways;  // Tracks all connected hallways
    private List<Node> connectedNodes;        // Other connections (stairs, elevators)

    public Intersection(String name, Hallway primaryHallway, int positionAlongHallway, int floor) {
        super(name, primaryHallway, positionAlongHallway, floor);
        this.connectedHallways = new ArrayList<>();
        this.connectedNodes = new ArrayList<>();
        
        if (primaryHallway != null) {
            this.connectedHallways.add(primaryHallway);
        }
    }

    // Hallway connections
    public void addHallway(Hallway hallway) {
        if (!connectedHallways.contains(hallway)) {
            connectedHallways.add(hallway);
        }
    }

    public List<Hallway> getConnectedHallways() {
        return connectedHallways;
    }

    // Other node connections (stairs, elevators)
    public void addConnectedNode(Node node) {
        if (!connectedNodes.contains(node)) {
            connectedNodes.add(node);
        }
    }

    public List<Node> getConnectedNodes() {
        return connectedNodes;
    }

    @Override
    public void displayInfo() {
        System.out.println("Intersection: " + name + " at " + positionAlongHallway + "m along " + 
                          (hallway != null ? hallway.name : "no assigned hallway"));
        
        System.out.println("Connected Hallways:");
        for (Hallway h : connectedHallways) {
            System.out.println("  - " + h.name);
        }
        
        System.out.println("Connected Nodes:");
        for (Node n : connectedNodes) {
            System.out.println("  - " + n.name);
        }
    }
}
/*

    @Override
    public void displayInfo() {
        System.out.println("Once at Intersection: " + name + " at " + positionAlongHallway + "m along " + hallway.name);
      
    }  
        
    */



    /*     @Override
    public void displayInfo() {
        System.out.println("Intersection: " + name + " at " + positionAlongHallway + "m along " + hallway.name);
        System.out.println("Connected Nodes:");
        for (Node node : connectedNodes) {
            System.out.println("  - " + node.name);
        }
    }   
        
    
}
*/