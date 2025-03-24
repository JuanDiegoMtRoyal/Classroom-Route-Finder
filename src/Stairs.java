import java.util.*;

public class Stairs extends Node {
    private List<String> connectedFloors;

    public Stairs(String name, Hallway hallway, int positionAlongHallway, int floor) {
        super(name, hallway, positionAlongHallway, floor);
        this.connectedFloors = new ArrayList<>();
    }

    public void addConnectedFloor(String stairName) {
        connectedFloors.add(stairName);
    }

    @Override
    public void displayInfo() {
        System.out.println(" Take Stairs: " + name + " at " + positionAlongHallway + "m along " + hallway.name);
    }
}