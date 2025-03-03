public class Main2 {
    public static void main(String[] args) {
        // Create the graph
        Graph graph = new Graph();

        // Parse the building map
        Parser parser = new Parser(graph);
        parser.initializeBuildingMap("data/bissetFloor1.txt");

        // Create the Classroom Route Finder
        CRF crf = new CRF(graph);

        // Start user input handler
        UserInput userInput = new UserInput(crf);
        userInput.start();
    }
}
