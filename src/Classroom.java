public class Classroom extends Node{

        String compassDirection;
    
        public Classroom(String name, Hallway hallway, int positionAlongHallway, String compassDirection, int floor) {
            super(name, hallway, positionAlongHallway, floor);
            this.compassDirection = compassDirection;
        }
    
        @Override
        public void displayInfo() {
            System.out.println("Classroom: " + name + " at " + positionAlongHallway + "m along " + hallway.name);
        }
    
}
