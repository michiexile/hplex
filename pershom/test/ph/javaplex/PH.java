//import edu.stanford.math.plex.*;
import java.io.*;
import java.util.Scanner;
import java.util.Iterator;
import edu.stanford.math.plex4.api.*;
import edu.stanford.math.plex4.metric.impl.*;
import edu.stanford.math.plex4.streams.impl.*;
import edu.stanford.math.plex4.homology.chain_basis.*;
import edu.stanford.math.plex4.homology.interfaces.AbstractPersistenceAlgorithm;
import edu.stanford.math.plex4.homology.barcodes.*;
//import edu.stanford.math.plex4.streams.impl.ExplicitSimplexStream;
//import edu.stanford.math.plex4.streams.impl.VietorisRipsStream;
//import edu.stanford.math.plex4.streams.impl.ExplicitStream;
//import edu.stanford.math.plex4.homology.chain_basis.Simplex;

class PH
{
    public static void failArgs()
    {
	System.err.println("Specify precisely these arguments: the number of points in the cloud, the dimension of Euclidean space from which the points are sampled, the maximum scale of the VR complex, the maximum dimension of the simplices, the number of subdivisions");
	System.exit(1);
    }

    public static void main(String[] args)
    {
	int n = -1;
	int m = -1;
	double scale = -1;
	int d = -1;
	int N = -1;

	if (args.length != 5)
	    failArgs();

	try
	{
	    n = Integer.parseInt(args[0]);
	    m = Integer.parseInt(args[1]);
	    scale = Double.parseDouble(args[2]);
	    d = Integer.parseInt(args[3]);
	    N = Integer.parseInt(args[4]);
	}
	catch (NumberFormatException e)
	{
	    failArgs();
	}

	double[][] points = new double[n][m];
	Scanner sc = new Scanner(System.in);
	
	int i = 0;
	while (sc.hasNextDouble())
	{
	    for (int j = 0; j < m; j++)
	    {
		points[i][j] = sc.nextDouble();
	    }

	    i++;
	}

	System.err.println("Computing!");
	EuclideanMetricSpace space = Plex4.createEuclideanMetricSpace(points);
	VietorisRipsStream stream = Plex4.createVietorisRipsStream(points, d, scale, N);
	
	stream.finalizeStream();

	AbstractPersistenceAlgorithm<Simplex> persistence = Plex4.getDefaultSimplicialAlgorithm(d);
	BarcodeCollection<Integer> barcode = persistence.computeIntervals(stream);
	System.out.println(barcode);

	// Iterator<Simplex> it = stream.iterator();
	// while (it.hasNext())
	// {
	//     System.out.println(it.next());
	// }
	
	//Plex4.createExplicitSimplexStream();
	//Plex4.createVietorisRipsStream(points, 1, 1, 1);
	//VietorisRipsStream stream = Plex4.createVietorisRipsStream();
    }
}