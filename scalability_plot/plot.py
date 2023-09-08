import math

cell_size_bytes = 256
# starting tokens per bucket
MAX_TKNS = 2
# num pending tokens to send at each node
TKN_QUEUE_LEN = 4
KB = 1024
MB = 1024 * KB
GB = 1024 * MB
shoal_feedback_bits = 11 + 64 + 1


shoal_max_queue_len = dict()
shale_max_active_buckets_h4 = dict()
shale_max_active_buckets_h2 = dict()
pieo_list_size = dict()

# 256
shoal_max_queue_len[256] = 10
shale_max_active_buckets_h4[256] = 64
shale_max_active_buckets_h2[256] = 512
pieo_list_size[256] = 25

# 1024
shoal_max_queue_len[1024] = 20
shale_max_active_buckets_h4[1024] = 128
shale_max_active_buckets_h2[1024] = 1024
pieo_list_size[1024] = 64

# 4096
shoal_max_queue_len[4096] = 40
shale_max_active_buckets_h4[4096] = 256
shale_max_active_buckets_h2[4096] = 2048
pieo_list_size[4096] = 100

# 16384
shoal_max_queue_len[16384] = 80
shale_max_active_buckets_h4[16384] = 512
shale_max_active_buckets_h2[16384] = 8192
pieo_list_size[16384] = 225

def get_shale_memory_reqs(N, h, A, P):
	# -------- shale memory reqs ----------
	nodes_per_phase = round(math.pow(N , 1/h))
	phase_size = nodes_per_phase - 1
	epoch_size = h * phase_size
	num_buckets = (N * h) + 1

	phase_bits = math.ceil(math.log(h, 2))
	slot_bits = math.ceil(math.log(phase_size, 2))

	bucket_idx_bits = math.ceil(math.log(num_buckets, 2))

	# bucket mapping, free list, reverse mapping, in that order
	mem_opt_map_bits = (num_buckets * bucket_idx_bits) + A + (A * bucket_idx_bits)
	mem_opt_map_bytes = mem_opt_map_bits / 8.0

	# direct & spray buffers
	fwd_bufs_bytes = 2 * (h * A * phase_size) * cell_size_bytes

	# PIEO mem usage - 2 * num items in pieo
	# bucket_id, spray_hops_recvd, prev_hop_phase, prev_hop_slot, rank (not needed)
	pieo_element_bits = bucket_idx_bits + phase_bits + phase_bits + slot_bits + 1
	pieo_size_bytes = (pieo_element_bits * P * 2) / 8.0

	# token count map
	tkn_count_bits = epoch_size * A * math.ceil(math.log(MAX_TKNS, 2))
	tkn_count_bytes = tkn_count_bits / 8.0

	# token queue length
	tkn_queues_bytes = (TKN_QUEUE_LEN * epoch_size * bucket_idx_bits)

	return fwd_bufs_bytes, mem_opt_map_bytes, pieo_size_bytes, tkn_count_bytes, tkn_queues_bytes


def compare_memory_reqs(N, h, A, P, shoal_queue_len):

	fwd_bufs_bytes, mem_opt_map_bytes, pieo_size_bytes, tkn_count_bytes, tkn_queues_bytes = get_shale_memory_reqs(N, h, A, P)
	total_shale = fwd_bufs_bytes + mem_opt_map_bytes + pieo_size_bytes + tkn_count_bytes + tkn_queues_bytes

	# ---------- shoal memory reqs ---------

	node_bits = math.ceil(math.log(N, 2))

	fwd_bufs_bytes = shoal_queue_len * N * cell_size_bytes

	feedback_matrix_bytes = (N * N * shoal_feedback_bits) / 8.0

	feedback_queues_bytes = (N * node_bits) / 8.0

	total_shoal = fwd_bufs_bytes + feedback_matrix_bytes + feedback_queues_bytes

	print("Shale:", total_shale, "B")
	print("Shoal:", total_shoal, "B")
	return total_shale/GB, total_shoal/GB


N_values = [256, 1024, 4096, 16384]
shoal_mem = []
shale_h4_mem = []
shale_h2_mem = []
# h = 4
for N in N_values:
	print(N, "nodes")
	shale, shoal = compare_memory_reqs(N, 4, shale_max_active_buckets_h4[N], pieo_list_size[N], shoal_max_queue_len[N])
	shoal_mem.append(shoal)
	shale_h4_mem.append(shale)

# h = 2
for N in N_values:
	print(N, "nodes")
	shale, shoal = compare_memory_reqs(N, 2, shale_max_active_buckets_h2[N], pieo_list_size[N], shoal_max_queue_len[N])
	shale_h2_mem.append(shale)

import matplotlib.pyplot as plt

plt.plot(N_values, shale_h2_mem, '-o', label='shale h=2')
plt.plot(N_values, shale_h4_mem, '-o', label='shale h=4')
plt.plot(N_values, shoal_mem, '-o', label='shoal')
plt.title('Scalability: Shale vs Shoal')
plt.legend()
plt.xlabel("N")
plt.ylabel("Mem Req (GB)")
plt.show()


fwd_cells_mem = []
sched_state_mem = []

for N in N_values:
	fwd_bufs_bytes, mem_opt_map_bytes, pieo_size_bytes, tkn_count_bytes, tkn_queues_bytes\
		 = get_shale_memory_reqs(N, 2, shale_max_active_buckets_h2[N], pieo_list_size[N])

	fwd_cells_mem.append((fwd_bufs_bytes + mem_opt_map_bytes)/ GB)
	sched_state_mem.append((pieo_size_bytes + tkn_count_bytes + tkn_queues_bytes) / GB)

plt.plot(N_values, fwd_cells_mem, '-o', label='cell buffers')
plt.plot(N_values, sched_state_mem, '-o', label='scheduling state')
plt.title('Shale h=2 Mem Breakdown')
plt.legend()
plt.xlabel("N")
plt.ylabel("Mem Req (GB)")
plt.show()