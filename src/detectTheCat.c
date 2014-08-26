#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv2/objdetect/objdetect.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/imgproc/imgproc_c.h>

#include <stdio.h>

IplImage* imgResize(IplImage* src, int min)
{
    
    IplImage* new;

    double p = min / 100;

    int width = src->width;
    int height = src->height;


    // landscape
    if (src->width > src->height) {

        if (src->width > min) {

            double f = (double)(src->width  - min) / p;
            height = (double)src->height / (f/100);
            width = min;
        }
    } else {
        if (src->height > min) {

            double f = (double)(src->height  - min) / p;
            height = (double)src->width / (f/100);
            width = min;
        }
    }

    new = cvCreateImage(cvSize(width, height), src->depth, src->nChannels);

    cvResize(src, new, CV_INTER_LINEAR);

    return new;
}

int detectTheCat(const char* filename, const char* model, float thre, int numThreads) {
    CvLatentSvmDetector *detector;
    CvMemStorage        *storage;
    CvSeq               *detect;
    IplImage            *image, *rescaled;
    
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

    rescaled = imgResize(image, 450);


    if ( !rescaled ) {
        printf("Could not resize image... (image: %s)\n", filename);
        cvReleaseImage( &image );
        cvReleaseLatentSvmDetector( &detector );
        cvReleaseMemStorage( &storage );
        return -1;
    }
    

    detect = cvLatentSvmDetectObjects(rescaled, detector, storage, 0.0000001f, numThreads);

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
    cvReleaseImage( &rescaled );
    
    return isFound;
}
