#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv2/objdetect/objdetect.hpp>

#include <stdio.h>
int detectTheCat(const char* filename, const char* model, float thre, int numThreads) {
    CvLatentSvmDetector *detector;
    CvMemStorage        *storage;
    CvSeq               *detect;
    IplImage            *image;
    
    int                 isFound = 0;
    unsigned int        i;

    detector     = cvLoadLatentSvmDetector(model);

    if( !detector ) {
        printf("Could not load detector: %s\n", model);
        return -1;
    }

    storage      = cvCreateMemStorage(0);

    if( !storage ) {
        cvReleaseLatentSvmDetector( &detector );
        return -1;
    }

    image        = cvLoadImage(filename, CV_LOAD_IMAGE_COLOR);

    if( !image ) {
        printf("Could not load image... (image: %s)\n", filename);
        cvReleaseLatentSvmDetector( &detector );
        cvReleaseMemStorage( &storage );
        return -1;
    }

    detect = cvLatentSvmDetectObjects(image, detector, storage, 0.0000001f, numThreads);

    for( i = 0; i < detect->total; i++ ) {

        CvObjectDetection detection = *(CvObjectDetection*)cvGetSeqElem( detect, i );

        if( detection.score > thre ) {
            isFound = 1;
            break;
        }
    }

    cvReleaseLatentSvmDetector( &detector );
    cvReleaseMemStorage( &storage );
    cvReleaseImage( &image );
    
    return isFound;
}
