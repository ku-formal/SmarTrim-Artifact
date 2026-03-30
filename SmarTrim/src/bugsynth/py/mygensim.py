from gensim.models import KeyedVectors
import os
import gensim.downloader

wv_path = os.path.join(os.getcwd(), 'resource/pretrained', 'fastText.kv')

def download_fastText():
    wv = gensim.downloader.load('fasttext-wiki-news-subwords-300')
    wv.save(wv_path)

def load_fastText():
    wv = KeyedVectors.load(wv_path)
    return wv

def contain(wv, x):
    assert(type(x) == str)
    return wv.__contains__(x)
    # return wv.has_index_for(x) # XXX

def similarity(wv, a, b):
    assert(type(a) == str and type(b) == str)
    return wv.similarity(a,b)
