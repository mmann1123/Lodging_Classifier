import matplotlib.pyplot as plt

def show_images(images, cmap=None):
    if len(images) == 0:
	return 'No images available'
    cols = 2
    rows = (len(images)+1)//cols
    
    plt.figure(figsize=(20, 21))
    for i, image in enumerate(images):
        plt.subplot(rows, cols, i+1)
        # use gray scale color map if there is only one channel
        cmap = 'gray' if len(image.shape)==2 else cmap
        plt.imshow(image, cmap=cmap)
        plt.xticks([])
        plt.yticks([])
    plt.tight_layout(pad=0, h_pad=0, w_pad=0)
    plt.show()
