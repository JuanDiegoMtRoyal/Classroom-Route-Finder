import java.util.*;

abstract class Node{
String name;
int floor;
Hallway hallway;
int positionAlongHallway;

public Node(String name, Hallway hallway, int positionAlongHallway, int floor)
{
    this.name = name;
    this.hallway = hallway;
    this.positionAlongHallway = positionAlongHallway;
    this.floor = floor;
}

public abstract void displayInfo();


}