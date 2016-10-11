cmake_minimum_required(VERSION 2.6)
cmake_policy(SET CMP0011 NEW)

project("AriaProject")

set(ARCB_MODULE_PATH 
		"${PROJECT_SOURCE_DIR}/cmake/arcb/cmake"
		CACHE INTERNAL "the cmake module path to arcb")

set(CMAKE_MODULE_PATH 
	${CMAKE_MODULE_PATH} 
	${ARCB_MODULE_PATH}
	"${PROJECT_SOURCE_DIR}/cmake/"
	)

set(projects 
	"AriaTest"
	"ariaracer"
	)

foreach(p ${projects})
	add_subdirectory(${p})
endforeach(p)
