import {useQuery, useMutation, useQueryClient, QueryClient, QueryClientProvider,} from '@tanstack/react-query'

import { NavBar } from "./components";
import { TodoApp } from "./components";

const queryClient = new QueryClient()

function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <>
        <NavBar />
        <TodoApp
          initialFilter={"All"}/>
      </>
    </QueryClientProvider>
  )
}

export default App;