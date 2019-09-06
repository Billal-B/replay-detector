"""
Process an image that we can pass to our networks.
"""
from keras.preprocessing.image import img_to_array, load_img
import numpy as np

def process_image(image_name, target_shape):
    """Given an image, process it and return the array."""
    # Load the image.
    h, w, _ = target_shape
    image = load_img(image_name, target_size=(h, w))
    img_arr = img_to_array(image)

    # Turn it into numpy, normalize and return.
    if img_arr.shape != (80,80,3):
        print(">>>>>>>>>>>>>>>>>>>>>> ERROR")
        print(image_name)
        print(img_arr.shape)
    x = (img_arr / 255.).astype(np.float32)

    return x
