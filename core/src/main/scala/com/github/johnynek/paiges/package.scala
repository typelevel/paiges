package com.github.johnynek

package object paiges {

  type StreamTree[T] = Stream[Either[(T, () => T), Chunk]]

  type DocTree = Fix[StreamTree]
}
