#!/usr/local/bin/python3

# find entries with duplicate paths from inside match docker
# code based on https://www.elastic.co/blog/how-to-find-and-remove-duplicate-documents-in-elasticsearch

from collections import defaultdict
from elasticsearch import Elasticsearch

es = Elasticsearch(["elasticsearch:9200"])

all_paths = set()
duplicate_paths = defaultdict(list)
duplicate_ids = []

def process_chunk(hits):
    for hit in hits:
        path = hit['_source']['path']
        if path in all_paths:
            duplicate_paths[path].append(hit['_id'])
            duplicate_ids.append(hit['_id'])
        else:
            all_paths.add(path)

def collect_duplicate_paths():
    data = es.search(index="images", scroll='1m',  body={"query": {"match_all": {}}})
    sid = data['_scroll_id']
    scroll_size = len(data['hits']['hits'])
    process_chunk(data['hits']['hits'])
    while scroll_size > 0:
        data = es.scroll(scroll_id=sid, scroll='2m')
        process_chunk(data['hits']['hits'])
        sid = data['_scroll_id']
        scroll_size = len(data['hits']['hits'])

def delete_duplicate_ids():
    return es.delete_by_query(index="images", body={"query": {"ids": {"values": duplicate_ids}}})


def dump_all_paths():
    with open('all_paths.txt', 'w') as f:
        for path in all_paths:
            print(path, file=f)


if __name__ == '__main__':
    collect_duplicate_paths()
    delete_duplicate_ids()
    dump_all_paths()
